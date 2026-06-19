# Session summary — caco-web auth Slice 1 (crypto/bootstrap foundation)

## Goal

Land the first, foundational slice of the durable caco-web dashboard auth (the
fix for the bd-83db05 unauthenticated-proxy exposure). Slice 1 is additive
crypto + bootstrap helpers ONLY — no server wiring or request-time enforcement
yet (those are later slices) — so it carries zero behavioral risk while
establishing the security primitives the rest of the auth is built on.

## Bead(s)

- `bd-2f7e03` — caco-web auth Slice 1: crypto/bootstrap foundation
- parent (kept OPEN): the caco-web real-auth child of the security epic; Slices
  2-6 (config/startup posture, enforcement middleware + login, WS gating, SPA
  login view + auto-credential, docs) remain.

## Before state

- Failing tests: none. caco-web had no auth primitives; no `auth` module.
- Design + crypto spec fully reviewed and approved (daemon-side by msm-3 over two
  rounds + controller greenlight). Slice 1 implementation reviewed and
  approved-to-land by msm-3, with one forward note (handle host:port in the
  loopback check) which is incorporated here.

## After state

- Failing tests: none. `cargo test -p caco-web --lib` green (15 auth tests) and
  `cargo check --workspace --tests` green (exit 0) — the new deps add no
  cross-crate breakage.
- New `crates/caco-web/src/auth.rs` (additive, unenforced):
  - `resolve_dashboard_token` — configured wins; loopback auto-gen+persist
    (zero-config local); non-loopback + no token → `RefuseToStart`.
  - `is_loopback_bind` — handles bare host AND `host:port` (the refuse-to-start
    gate cannot misfire on a ported loopback bind).
  - `ensure_secret_file` — atomic `O_CREAT|O_EXCL` 0600 + `sync_all` +
    read-on-race retry, mirroring `caco-daemon` `token.rs::ensure_token`.
  - `load_or_create_cookie_key` — persisted, restart-stable, independent of the
    access token.
  - `sign/verify_session_cookie` — real `Hmac<Sha256>` (deliberately not the
    length-extension-vulnerable provenance keyed-hash), versioned domain prefix
    `b"caco-web-session-cookie-v1\0"`, constant-time verify, MAC-before-parse,
    expiry check.

## Diff summary

- Code commit: pending final squash SHA from the reintegration receipt.
- Files: `crates/caco-web/src/auth.rs` (new), `crates/caco-web/src/lib.rs`
  (`pub mod auth;`), `crates/caco-web/Cargo.toml` (hmac 0.12 + base64 0.22 +
  workspace sha2/rand).
- Tests: +16 unit tests (token precedence/autogen/converge/0600/refuse,
  is_loopback bare + host:port, cookie roundtrip/restart-stable/expired/
  tampered-mac/tampered-payload/wrong-key/malformed, cookie-key persist,
  token_matches). No behavioral change to the running server.

## Embedded artefacts

None (additive Rust crypto module; no UI surface).

## Operator-takeaway

The security-critical crypto/bootstrap foundation is in and fully unit-tested,
with msm-3's daemon-side review on both the design and the implementation. It is
deliberately unenforced (no request gating yet) so it lands risk-free; the
behavior change (auth-on) comes in the enforcement slice, where the
strip-browser-credential + guard-the-proxy-passthrough + WS-cookie-on-upgrade
work lands with another msm-3 review. The parent auth bead stays open until the
full auth is live and validated.
