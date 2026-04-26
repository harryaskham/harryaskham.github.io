# Session summary — bound caco-web Summaries loading

## Goal

Turn the caco-web Session Summaries route from an indefinite loading state into an explicit, retryable degraded state when the daemon-side summaries API hangs. The operator-facing goal was to preserve a clean browser console and healthy web shell while making the stuck route explain what happened instead of spinning forever.

## Bead(s)

- `bd-79ea46` — caco-web Summaries view hangs on pending /api/v1/summaries

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: current-assets caco-web observation reproduced `/api/v1/summaries?limit=200&offset=0` hanging for more than 18 seconds while `/health`, `/api/v1/ui/snapshot`, and `/api/v1/merge-queue?since=24h` returned `200 OK`.
- Context: the `#summaries` route stayed on `Loading summaries…` with no operator-facing error and no console errors. Evidence was captured in `/tmp/caco-web-summaries-duty-210146-observation.log` and `.playwright-cli/page-2026-04-26T20-02-31-348Z.png`.

## After state

- Failing tests: none observed.
- Relevant metrics: direct summaries probe now returns a handled `200 OK` JSON sentinel in about 8 seconds with `X-Caco-Upstream-Status: 504`; the Summaries route renders `Session summaries unavailable: daemon proxy timed out` plus a retry button; `/health` remains `200 OK`; browser console remains `0` errors and `0` warnings.
- Context: after rebasing onto current `origin/main`, the targeted regression `summaries_proxy_and_view_are_bounded_bd_79ea46` still passed against caco-web `v1.2.565`.

## Diff summary

- Commits: `cd75618a6` (`bd-79ea46: bound caco-web summaries loading`).
- Files touched: `crates/caco-web/src/proxy.rs`, `crates/caco-web/static/summaries.js`, `crates/caco-web/src/tests.rs`.
- Tests: added one focused caco-web regression contract for bounded summaries proxy/view handling; no tests removed.
- Behavioural delta: caco-web now applies an 8-second proxy timeout to `GET /api/v1/summaries` and `GET /api/v1/summaries/...`; timeout responses use the existing `200 OK` backend-unavailable sentinel shape to avoid browser resource-error noise. `summaries.js` classifies those sentinels for both list and detail fetches and renders the existing retryable error state instead of leaving the view indefinitely loading.
- Validation: `node --check crates/caco-web/static/summaries.js`; `git diff --check`; `cargo fmt --all -- --check`; `CARGO_BUILD_JOBS=2 cargo test -p caco-web --lib summaries_proxy_and_view_are_bounded_bd_79ea46`; `CARGO_BUILD_JOBS=2 cargo build -p caco-web --bin caco-web-dev-server`; lightweight Playwright proof in `/tmp/caco-web-bd-79ea46-final-211438-validation.log` with screenshot `.playwright-cli/page-2026-04-26T20-15-06-058Z.png`; `CARGO_BUILD_JOBS=2 cargo check -p caco-web --all-targets`; `CARGO_BUILD_JOBS=2 cargo test -p caco-web --lib` (286 passed); post-rebase targeted regression rerun passed.

## Operator-takeaway

The Summaries surface now degrades explicitly under daemon summaries slowness: operators see a retryable timeout message instead of a permanent loading spinner, and Playwright/browser-console sweeps stay quiet for this handled failure mode.
