# Session summary — snapshot timeouts are not full backend outages

## Goal

Handle the operator-reported caco-web state where the dashboard says “Backend unavailable” even though the daemon and web shell are up, because the heavyweight `/api/v1/ui/snapshot` request is merely timing out. Also tighten the persistent caco-web duty-cycle instructions so future cycles search for unlabeled caco-web-related beads rather than relying only on labels.

## Bead(s)

- `bd-d78de9` — caco web: shows "dashboard backend unavialble" even tho deamon and caco web are up

## Before state

- Failing tests: none at implementation start.
- Relevant metrics: `caco web status` reported healthy on port `11180`; a direct dev-server snapshot probe returned HTTP `200 OK` after about 8.1s with `X-Caco-Upstream-Status: 504` and a handled `daemon_proxy_timeout` sentinel.
- Context: the browser rendered `Backend unavailable` / `Dashboard backend unavailable…` for a slow bulk snapshot, which made a reachable daemon and live web process look like a full outage.
- Evidence: bead description cited `[caco-web] GET /api/v1/ui/snapshot -> 200 8002ms` and a dev-console snapshot delay; local validation reproduced the timeout sentinel in `/tmp/caco-web-bd-d78de9-233632-validation.log`.

## After state

- Failing tests: none observed after the fix.
- Relevant metrics: Playwright validation at `390x844` now reports `topConnection: "Snapshot delayed"`, `heroConnection: "Snapshot delayed…"`, `heroConnectionClass: "hero-pill snapshot_degraded"`, console `0` errors / `0` warnings, while the direct snapshot request still returns the bounded timeout sentinel in about 8s.
- Context: the UI now distinguishes a slow bulk snapshot from a true backend outage. Existing 5xx/authorization/backend-unavailable handling remains tested separately.
- Evidence: `/tmp/caco-web-bd-d78de9-233632-validation.log`, screenshot `.playwright-cli/page-2026-04-26T22-37-02-266Z.png`.

## Diff summary

- Commits: `03cfd42d6` (`bd-d78de9: distinguish snapshot timeout state`).
- Files touched: `.cacophony/profiles/caco-web.md`, `crates/caco-web/static/app.js`, `crates/caco-web/static/style.css`, `crates/caco-web/src/tests.rs`.
- Tests: added `app_js_labels_snapshot_proxy_timeout_as_degraded_bd_d78de9`; updated existing snapshot timeout/backend-unavailable source assertions to account for the new `snapshot_degraded` branch.
- Behavioural delta: `/api/v1/ui/snapshot` `daemon_proxy_timeout` sentinels now set a `snapshot_degraded` connection state with “Snapshot delayed” copy and warning styling instead of generic “Backend unavailable”. The profile’s active duty cycle now explicitly requires text/title searches for unlabeled `caco web`, dashboard, browser, workspace, webui, summaries, visual-polish, and Playwright beads.
- Validation: `node --check crates/caco-web/static/app.js`; `git diff --check`; `cargo fmt --all -- --check`; focused snapshot tests; Playwright reproduction; `CARGO_BUILD_JOBS=2 cargo check -p caco-web --all-targets`; `CARGO_BUILD_JOBS=2 cargo test -p caco-web --lib` (294 passed); post-rebase focused regression rerun passed.

## Operator-takeaway

A slow or overloaded bulk snapshot no longer makes caco-web look fully down: operators now see “Snapshot delayed” while the dashboard retries, which better matches the real state when the daemon and web shell are alive but the heavyweight snapshot path is slow.
