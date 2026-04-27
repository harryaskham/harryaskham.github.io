# Session summary — Console-clean daemon unavailable state

## Goal

Keep the caco-web browser dashboard trustworthy during daemon restart/unavailable windows. This duty cycle observed that the UI rendered a handled degraded state, but Chromium still reported red resource-load errors for expected daemon 503s; the goal became to make those handled backend-unavailable paths console-clean without hiding the operator-facing degraded state.

## Bead(s)

- `bd-c2310e` — caco-web logs handled daemon 503s as browser console errors during restart

## Before state

- Failing tests: none at start of implementation.
- Relevant metrics: duty-cycle observation `/tmp/caco-web-duty-current-001255-observation.log` reported `Total messages: 8 (Errors: 8, Warnings: 0)` from 503 responses during a daemon restart/unavailable window.
- Context: `caco web status` recovered to healthy on `11180`, but local daemon/bead authority had just flapped. The current-assets helper showed handled `Backend unavailable` / `Snapshot delayed` UI while DevTools still logged `Failed to load resource: 503` for snapshot, merge-queue, UI stream, and logs stream endpoints.

## After state

- Failing tests: none observed in the caco-web validation lane.
- Relevant metrics: fresh unavailable-daemon Playwright validation `/tmp/caco-web-bd-c2310e-validation-fresh.log` reported `Total messages: 0 (Errors: 0, Warnings: 0)` while still rendering `Backend unavailable`.
- Context: caco-web proxy now maps read-only daemon 503/connect-unavailable paths to HTTP 200 handled sentinels, with SSE-shaped sentinel events for EventSource clients, so browser DevTools stays clean while app.js continues showing explicit degraded/retry UI.

## Diff summary

- Commits: `7842731ae`
- Files touched: `crates/caco-web/src/proxy.rs`, `crates/caco-web/src/tests.rs`
- Tests: +1 regression test / -0 tests / flipped 0 tests
- Behavioural delta: caco-web now translates expected read-only daemon unavailable responses into console-clean sentinels instead of forwarding 503s directly to the browser. Snapshot timeout behaviour remains unchanged, and SSE consumers get a one-event `event: error` stream with backend-unavailable metadata.
- Validation: `cargo fmt --all`; `CARGO_BUILD_JOBS=2 cargo check -p caco-web --all-targets`; `CARGO_BUILD_JOBS=2 cargo test -p caco-web --lib` — 296 passed; fresh Playwright unavailable-daemon validation at `/tmp/caco-web-bd-c2310e-validation-fresh.log` — 0 console errors / 0 warnings.

## Embedded artefacts

- `/tmp/caco-web-duty-current-001255-observation.log` — before evidence showing handled UI plus browser 503 console errors during daemon unavailability.
- `/tmp/caco-web-bd-c2310e-validation-fresh.log` — after evidence showing handled `Backend unavailable` UI with zero browser console errors.
- `.playwright-cli/page-2026-04-27T00-23-18-957Z.png` — after Workspace screenshot in the unavailable-daemon validation pass.
- `.playwright-cli/page-2026-04-27T00-25-17-958Z.png` — after help-overlay screenshot from the same validation pass.

## Operator-takeaway

The dashboard still tells the operator when the daemon is unavailable, but expected restart-window 503s no longer pollute the browser console as red errors. A follow-up draft `bd-bfc2b8` captures the validation friction where `caco-web-observe --skip-build` can accidentally reuse a stale dev-server binary after proxy changes.
