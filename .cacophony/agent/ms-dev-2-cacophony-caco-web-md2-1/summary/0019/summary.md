# Session summary — caco-web auth Slice 4 (the enforce_auth flip — completes bd-764efa)

## Goal

Turn request enforcement default-ON for caco-web when a dashboard token is
configured, so a configured dashboard is never served unauthenticated (the
slice-3 login UX handles access), completing the bd-764efa exposure fix. An
unconfigured launch (no resolvable token) stays open; `CACO_WEB_ENFORCE_AUTH` is
an explicit override either way.

## Bead(s)

- `bd-a482f2` — caco-web auth Slice 4 (final): enforce_auth default-on flip
- parent `bd-e5f22f` (the auth bead) + epic `bd-764efa` — this slice completes them.

## Before state

- Failing tests: none. The full auth backend + login UX (S1→Slice 3) landed inert;
  enforcement was off by default (`enforce_auth: false` / `CACO_WEB_ENFORCE_AUTH`
  opt-in only), so a configured dashboard could still be served unauthenticated.

## After state

- Failing tests: none. Build green; flip unit test `flip_default_on_when_configured`
  green; caco-web `auth_middleware` / `login_endpoint` tests green.
- `caco-cli`: `resolve_web_enforce_auth(env, dashboard_configured)` — explicit
  `CACO_WEB_ENFORCE_AUTH` wins, otherwise enforce iff a dashboard token + cookie key
  resolved. Computed before the WebConfig literal moves the credentials.
- Edge-fix (msm-3 hard-gate catch): the dispatch_web no-tokens-dir branch now
  refuses ALL non-loopback binds (not just no-token), since without a tokens dir
  there is no cookie key to enforce with — closing a path where a configured token
  + no dir would have served an OPEN dashboard off-loopback (the middleware passes
  through when cookie_key is absent). Parity with the token refuse-to-start.
- 3-quadrant coupling closes bd-764efa: off-loopback+no-token → REFUSE-to-start;
  off-loopback+token → ENFORCE; loopback+auto-gen → ENFORCE + ?token auto-login.

## Diff summary

- Code commit: pending final squash SHA from the reintegration receipt.
- Files: `crates/caco-cli/src/lib.rs` (helper + wiring + unit test).
- Tests: +1 unit (`flip_default_on_when_configured`).
- Behavioural delta: a configured caco-web now enforces auth by default; the login
  UX (form + ?token) keeps it ergonomic; off-loopback without a token refuses to boot.

## Embedded artefacts

- `web/screenshots/slice4-enforce-form-login.png` — enforce-on login → dashboard.
- `web/screenshots/slice4-enforce-autologin.png` — ?token auto-login under enforcement.

## Operator-takeaway

This flip completes the caco-web auth arc (bd-764efa): the dashboard can no longer
be served unauthenticated off-loopback (refuse-to-start), and a configured one
enforces a signed session cookie on every `/api/*` request (including the SSE
stream and PTY websocket), with a zero-config loopback `?token` auto-login and a
universal login form so it never locks itself out. Validated by a full 6-case
enforce-on e2e against a real local daemon, reviewed by msm-3 across every slice.
`CACO_WEB_ENFORCE_AUTH=0` is the explicit local-dev opt-out (not an IP bypass —
loopback stays enforced because a Funnel proxies to loopback).
