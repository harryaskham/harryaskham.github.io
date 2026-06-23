# Session summary — bd-8ae7ac: gate init fetches on the ?token auth bootstrap (web)

## Goal

Fix the bootstrap auth race found in the authenticated observation pass: the
?token -> /api/web/session cookie exchange is async, but the merge-queue badge
seed fired from a separate DOMContentLoaded listener without waiting, so it
401-raced the session cookie on first paint (a console error + possibly stale
first-paint data until re-navigation).

## Bead(s)

- `bd-8ae7ac` — caco-web ?token bootstrap init-ordering race fires authed fetches
  (merge-queue) before the session cookie => transient 401. Filed as a draft from
  the authenticated observation pass; promoted + claimed + implemented + closed
  this cycle.

## Before state

- Failing tests: none from this change at start.
- maybeBootstrapLogin() (consumes ?token, POSTs /api/web/session, sets the
  httpOnly cookie, no reload) is awaited in the main DOMContentLoaded handler, but
  startMergeQueuePoll() is a SEPARATE DOMContentLoaded listener that called
  loadMergeQueue() immediately without waiting for auth -> transient 401 on the
  fresh ?token load (verified live: 1x 401 on /api/v1/merge-queue at first paint;
  the endpoint itself returns 200 once the session is established).

## After state

- Failing tests: none. Full caco-web lib suite green (tj-88c21ba4: 716 passed, 0
  failed). The full-suite run (per gate discipline) caught a transient orphan
  window-export regression (js_window_exports_bd_0525ab) from an unnecessary
  window handle I added then removed.
- Live verified: 0 401s during a fresh ?token load (was 1); merge-queue view
  renders.
- app.js: added an `authBootstrapReady` promise resolved by markAuthBootstrapReady()
  right after `await maybeBootstrapLogin()` in the DOMContentLoaded handler, with a
  5s defensive setTimeout so a gated init fetch can never hang. The merge-queue
  initial seed now `authBootstrapReady.then(() => loadMergeQueue()...)` instead of
  racing it; the periodic poll (30s/2m later) is unaffected. Reusable primitive for
  any future init-time authed fetch.

## Diff summary

- Code commit: pending (final landed squash SHA from the reintegration receipt).
- crates/caco-web/static/app.js — authBootstrapReady promise + markAuthBootstrapReady()
  + defensive timeout; release after maybeBootstrapLogin; gate the merge-queue seed.
- crates/caco-web/src/tests.rs — +1 needle test
  (app_js_gates_init_fetches_on_auth_bootstrap_bd_8ae7ac).
- Tests: +1 needle test; full caco-web lib suite 716 passed / 0 failed.

## Embedded artefacts

- None (verified by a live 401-count probe on the fresh ?token load + the full
  unit suite; the fix is a console-cleanliness/first-paint-freshness gate with no
  new on-screen visual).

## Operator-takeaway

The ?token dashboard bootstrap no longer 401-races its own session cookie: the
merge-queue badge seed (and, via the reusable authBootstrapReady promise, any
future init-time authed fetch) now waits for the /api/web/session exchange before
firing, removing the first-paint 401 + console error and any stale-until-re-nav
data. Found and fixed in the same session that discovered + documented the
auth-default-on observation workflow itself.
