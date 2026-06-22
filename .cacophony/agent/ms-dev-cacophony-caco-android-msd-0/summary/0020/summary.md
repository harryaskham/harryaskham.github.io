# Session summary — bd-65f88f slice 2b: secret-free credential-name projection in release config

## Goal

Wire the daemon to expose a secret-free projection of configured credential
wrapper names (and app_stores) in the release config response, so the
`caco release trigger --dry-run` mobile-store credential guardrail (slice 2a)
populates with "configured" vs "BLOCKED" status. Daemon side of slice 2.

## Bead(s)

- `bd-65f88f` — caco release trigger: mobile app-store trigger semantics for
  Play/TestFlight (slice 2b; bead closes after the docs slice 2c).

## Before state

- Failing tests: none.
- The dry-run guardrail (slice 2a) could compute "BLOCKED" but the single-channel
  config response carried no app_stores or credential-presence data, so it
  always rendered the lighter (no-credential) form.

## After state

- Failing tests: none. `cargo test -p caco-daemon --lib release` = passed
  (tj-9d02e7b3, exit 0): real compile verified (`Compiling caco-daemon
  v1.2.1271`, ~12 min), 74 release tests passed.
- `handle_release_config` now includes `app_stores` and a secret-free
  `credentials` projection (the configured wrapper NAMES = keys of
  `config.credentials`, never the `CredentialWrapper` values) in both the
  single-channel and no-channel responses.

## Diff summary

- Code commits: bd-65f88f slice 2b; final landed squash SHA from the receipt.
- Files touched: `crates/caco-daemon/src/lib.rs` (credential_names projection +
  app_stores/credentials in both config response branches).
- Tests: covered by the caco-daemon release suite; the guardrail rendering is
  unit-tested on the CLI side (slice 2a).
- Behavioural delta: `caco release config` / the dry-run fetch now carry the
  configured credential wrapper names + app_stores. Secret-safe — names only,
  never secret material.

## Operator-takeaway

The mobile-store credential guardrail is now fully wired end-to-end: the daemon
exposes which credential wrappers exist (names only), and the dry-run plan shows
"configured" or "BLOCKED: credential not configured" accordingly — no secret ever
leaves the config. Last step is the docs slice (2c) defining the semantics and
failure modes, then bd-65f88f closes.
