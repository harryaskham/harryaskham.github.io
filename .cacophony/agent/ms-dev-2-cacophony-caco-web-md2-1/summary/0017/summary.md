# Session summary — caco-web auth Slice 2 (single-use URL-token bootstrap)

## Goal

Implement the loopback URL-token bootstrap (msm-3 design (i)): a separate,
single-use, in-memory bootstrap token delivered via the `caco web` launch URL and
consumed server-side on first exchange, so the durable dashboard token never
appears in a URL. Plus the two supporting hardenings (no-referrer header, `?token=`
log redaction). Reviewed + approved by msm-3.

## Bead(s)

- `bd-c7fafb` — caco-web auth Slice 2: single-use URL-token bootstrap + no-referrer
  + log redaction
- parent (kept OPEN): the caco-web real-auth child of the security epic; the SPA
  login form + strip-on-load (slice 3) and the `enforce_auth` flip remain.

## Before state

- Failing tests: none. The login endpoint accepted only the dashboard token; there
  was no zero-config local login path, no log redaction, no no-referrer header.

## After state

- Failing tests: none. `cargo test -p caco-web --lib` green (29/29);
  `cargo check --workspace --tests` + `cargo clippy -p caco-web -D warnings` green.
- `auth.rs`: `random_bootstrap_token`, `BOOTSTRAP_TOKEN_TTL_SECS` (300),
  `redact_url_token`.
- `ProxyState.bootstrap: Arc<Mutex<Option<(value,expiry)>>>`;
  `WebConfig.bootstrap_token`.
- `login_handler`: accept the durable dashboard token (reusable) OR a valid,
  unconsumed, unexpired bootstrap → consume on first exchange (atomic under one
  mutex hold) → mint cookie.
- `dispatch_web`: on a loopback bind + auth configured + interactive stderr (TTY),
  mint the bootstrap + print the one-time login link. The TTY guard keeps a
  credential-bearing URL out of a supervised service log (msm-3 note).
- `per_request_log_middleware`: `Referrer-Policy: no-referrer` on every response
  (h2) + `?token=` redaction in the logged URI (h3).

## Diff summary

- Code commit: pending final squash SHA from the reintegration receipt.
- Files: `crates/caco-web/src/{auth.rs,server.rs,proxy.rs,tests.rs}`,
  `crates/caco-cli/src/lib.rs`, `crates/caco-daemon/src/lib.rs`,
  `crates/caco-web/src/bin/caco-web-dev-server.rs`.
- Tests: +4 (bootstrap consume/reuse, no-referrer, redaction cases, token
  uniqueness).
- Behavioural delta: mostly inert-effect (enforce_auth still false → the cookie
  does nothing); live + safe: no-referrer header, log redaction, launcher URL.

## Embedded artefacts

None (Rust + integration tests).

## Operator-takeaway

The zero-config local login mechanism is built, reviewed (msm-3 verified atomic
single-use, constant-time, expiry, redaction, loopback-only mint), and landed
INERT. A `caco web` launch on loopback prints a one-time login link; the server
burns the token on first use; the durable token never touches a URL. The TTY guard
(folded in from msm-3's note) keeps the credential-bearing URL out of supervised
service logs. Remaining before enforcement turns on: the SPA login form +
strip-on-load (slice 3), then the `enforce_auth` flip (ping msm-3 first). The
supervised (non-TTY) zero-config login path is a slice-3 design item.
