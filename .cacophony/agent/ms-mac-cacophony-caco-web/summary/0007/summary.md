# Session summary — bounded caco-web snapshot proxy

## Goal

Stabilize caco-web after observation showed that a dashboard snapshot request could abort and coincide with the managed web service restarting, leaving the browser shell connected but dashboard data empty.

## Bead(s)

- `bd-fe4889` — caco-web snapshot request restarts managed web service

## Before state

- Failing tests: none known at session start.
- Relevant metrics: Playwright observation in `/tmp/caco-web-duty-191132-observation.log` captured repeated `/api/v1/ui/snapshot` `net::ERR_ABORTED` failures while SSE stayed `200 OK`. A direct managed caco-web probe returned `RemoteDisconnected` after 9.204s for `/api/v1/ui/snapshot`, then `/api/v1/node` immediately got connection refused; `caco web status` later showed a new caco-web PID.
- Context: `caco web` used the default Tokio runtime stack, unlike the daemon's explicit 16 MiB worker stack, and the web proxy used a broad 300s reqwest client timeout for all proxied requests, including the heavyweight UI snapshot.

## After state

- Failing tests: none in the targeted validation set.
- Relevant metrics: current-assets dev server direct `/api/v1/ui/snapshot` probe now returns HTTP 504 after about 8.071s, and `/health` remains 200 both immediately after the timeout and after Playwright use. Playwright showed the UI entering `Backend unavailable` instead of leaving an ambiguous connected/empty state.
- Context: caco-web production and dev entrypoints now use a shared explicit 16 MiB Tokio worker-stack runtime, and the snapshot proxy applies an 8s request timeout so it returns a bounded gateway timeout before the browser's 10s AbortController fires.

## Diff summary

- Commits: `1713a45df`
- Files touched: `SPEC.md`, `crates/caco-cli/src/lib.rs`, `crates/caco-web/src/lib.rs`, `crates/caco-web/src/proxy.rs`, `crates/caco-web/src/bin/caco-web-dev-server.rs`, `crates/caco-web/src/tests.rs`
- Tests: +2 regression tests / -0 / flipped 0
- Behavioural delta: caco-web no longer relies on platform/Tokio default worker stack for the dashboard proxy path, and slow snapshot upstream calls degrade to explicit 504 responses while the web service remains healthy.

## Embedded artefacts

- `/tmp/caco-web-bd-fe4889-193013-validation.log` — direct snapshot probe and lightweight Playwright validation for the bounded 504/health-stays-up behavior.
- `.playwright-cli/page-2026-04-26T18-30-45-754Z.png` — screenshot from the after-validation Playwright run.

## Operator-takeaway

The dashboard snapshot path is now fail-bounded at the caco-web layer: if the daemon snapshot stalls, operators should see backend-unavailable rather than a web-service restart or a browser-level aborted request with stale empty dashboard data.
