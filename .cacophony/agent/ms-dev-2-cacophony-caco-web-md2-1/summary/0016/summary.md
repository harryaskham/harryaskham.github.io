# Session summary — caco-web auth Slice 1 (enforcement middleware + proxy guard)

## Goal

Build the request-time auth enforcement middleware (gate `/api/*` + `/metrics` via
a valid session cookie or dashboard bearer) and the proxy.rs credential-passthrough
guard. Ships INERT (`enforce_auth` defaults false) so the dashboard behaves exactly
as before; enforcement is flipped on later once the login UX exists. Reviewed +
approved by msm-3 (daemon-side security reviewer).

## Bead(s)

- `bd-d6ca1f` — caco-web auth Slice 1: enforcement middleware + proxy.rs guard
- parent (kept OPEN): the caco-web real-auth child of the security epic; the
  URL-token bootstrap + SPA login form + the enforcement flip remain.

## Before state

- Failing tests: none. The login endpoint + decision helpers existed but nothing
  gated requests; `/metrics` + `/api/*` were open.

## After state

- Failing tests: none. `cargo test -p caco-web --lib` green (26/26, +2 middleware
  tests); `cargo check --workspace --tests` green.
- `dashboard_auth_middleware` (from_fn_with_state, inside cors, under
  per_request_log so 401s are logged): gates `/api/*` (except `/api/web/session`)
  AND `/metrics` via `auth::check_request_auth`; 401 JSON otherwise. Static pages
  + `/health` + login stay open. WS upgrades are under `/api/` → gated by
  cookie-on-upgrade.
- `enforce_auth` flag on WebConfig + ProxyState (default false) → middleware
  early-returns when not enforcing / unconfigured → ships inert.
- proxy.rs (both passthrough sites): forward the caller's Authorization to the
  daemon only when `dashboard_token.is_none()` (fail-safe; endorsed by msm-3).

## Diff summary

- Code commit: pending final squash SHA from the reintegration receipt.
- Files: `crates/caco-web/src/{server.rs,proxy.rs,tests.rs}`,
  `crates/caco-cli/src/lib.rs`, `crates/caco-daemon/src/lib.rs`,
  `crates/caco-web/src/bin/caco-web-dev-server.rs`.
- Tests: +2 middleware integration (enforced: 401 no-cred, 200 /health, /metrics
  401, login bypass, cookie+bearer pass, wrong-bearer 401; inert: never 401).
- Behavioural delta: none in the normal case (inert by default); the proxy guard
  only changes the degenerate `state.token=None` + `dashboard_token=Some` edge.

## Embedded artefacts

None (Rust + integration tests).

## Operator-takeaway

The dashboard's request-time auth gate is built, reviewed (msm-3 approved), and
landed INERT — it does nothing until `enforce_auth` is flipped on, which waits for
the login UX so the loopback dashboard can never lock itself out. msm-3's review
folded in two fixes: `/metrics` is now in the gated set (it leaks operational
metrics and must not be world-readable behind a Funnel), and the 401-logging order
(auth under per_request_log) was verified. Remaining before the flip: the in-memory
single-use URL-token bootstrap (+ `?token=` log redaction + `Referrer-Policy:
no-referrer`) and the SPA login form (+ strip-on-load).
