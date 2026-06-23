# Session summary — caco-web enforce_auth login-required connection state

## Goal

Fix a contradictory-UX bug in the enforce_auth dashboard (my bd-764efa/slice-3
work): when sign-in is required, the connection pill showed "Backend unavailable"
and fired a red error toast telling the operator to "restart caco web / unset
CACO_AGENT_TOKEN" — while the login form was showing. Introduce a distinct
sign-in state.

## Bead(s)

- `bd-8c6bba` — enforce_auth login-required reuses worker-token 'forbidden'.

## Before state

- Failing tests: none. enforce_auth on + logged out → snapshot/stream 401
  (x-caco-web-login:1). app.js showed the login form (correct) but
  `setConnectionStatus('forbidden')` reused the bd-b6ab99 worker-token-scope
  state: pill "Insufficient scope", diagnostic "bearer token lacks node scope.
  Restart caco web...", + an 8s error toast. The SSE 401 then overwrote it with
  'backend_unavailable' so the pill read "Backend unavailable". All wrong for a
  sign-in-required state. Verified live on my enforce-on instance.

## After state

- Failing tests: none. `node --check` OK; `cargo test -p caco-web --lib` passed.
- New `'login_required'` connection status: pill "Sign in required", diagnostic
  "Sign in to the dashboard to load data...", no error toast. A `state.loginRequired`
  latch (set on the marked-401, cleared on 'connected') is honored by a single
  centralized guard in `setConnectionStatus` that forces backend_unavailable/
  snapshot_degraded/disconnected/connecting → 'login_required' while logged out,
  so the SSE failure paths can't overwrite it. Verified live: pill = "Sign in
  required", correct tooltip, no worker-token toast, login form still shows.

## Diff summary

- Code commit: pending final squash SHA from the reintegration receipt.
- Files: `crates/caco-web/static/app.js` (5 edits: marked-401 path, centralized
  setConnectionStatus guard, don't-downgrade guard, pill case, diagnostic case).
- Tests: +0 (JS; existing needle tests assert preserved lines; caco-web lib green).
- Behavioural delta: enforce_auth sign-in now reads "Sign in required", not a
  contradictory "Backend unavailable / restart caco web".

## Embedded artefacts

- `web/screenshots/login-required-pill-fixed.png` — pill after the fix.

## Operator-takeaway

A degraded-state probe found my own enforce_auth UX (bd-764efa) reused the
worker-token 'forbidden' state for the sign-in case, producing a red "restart
caco web / bearer token lacks node scope" toast contradicting the login form. Now
a dedicated 'login_required' state reads "Sign in required" cleanly. All new
branches are gated on the loginRequired latch (only true when enforce_auth is on
AND logged out), so normal operation is unchanged. (Methodology note: now that
enforce_auth is default-on, probes must log in via the dashboard token / ?token
to reach data-bearing views.)
