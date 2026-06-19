# Session summary — caco-web auth Slice 3a (request-auth decision helpers)

## Goal

Add the request-auth DECISION logic for the caco-web dashboard auth — pure,
unit-tested helpers that decide whether a request is authenticated (signed
session cookie OR dashboard bearer) and mint secure session cookies. ADDITIVE:
not wired into request enforcement yet (the axum middleware + login endpoint is
slice 3b, gated on msm-3's review).

## Bead(s)

- `bd-e5fdaf` — caco-web auth Slice 3a: request-auth decision helpers
- parent (kept OPEN): the caco-web real-auth child of the security epic; 3b
  (wired middleware + login + proxy.rs:812 guard) + WS coverage remain.

## Before state

- Failing tests: none. auth.rs had the S1/S2 bootstrap + crypto but no
  request-level auth decision helpers.

## After state

- Failing tests: none. `cargo test -p caco-web --lib auth::` green (20/20,
  including 5 new).
- `crates/caco-web/src/auth.rs` adds (all pure, additive, unwired):
  `SESSION_COOKIE_NAME`, `session_cookie_value`, `bearer_from_authorization`,
  `check_request_auth` (accept valid signed cookie OR dashboard bearer; reject
  missing/wrong/expired; daemon bearer never involved), `DEFAULT_SESSION_TTL_SECS`,
  `build_set_cookie_header` (httpOnly + SameSite=Strict + Secure + Path=/).

## Diff summary

- Code commit: pending final squash SHA from the reintegration receipt.
- Files: `crates/caco-web/src/auth.rs` — 6 helper functions/consts + 5 unit tests.
- Tests: +5; additive, no behavioral change (the helpers are not yet called by
  any request path).
- Behavioural delta: none (decision helpers only; enforcement is 3b).

## Embedded artefacts

None (pure Rust helpers + tests).

## Operator-takeaway

The security-critical auth-decision logic (cookie/bearer validation + secure
session-cookie minting) is implemented and unit-tested in isolation, as
compositions of the already-reviewed S1 crypto primitives. It is deliberately
unwired so it lands risk-free; the WIRED enforcement (the axum middleware that
gates /api/* + the login endpoint + the proxy.rs:812 passthrough guard) is slice
3b and lands with msm-3's daemon-side review (msm-3 is currently offline in
transit).
