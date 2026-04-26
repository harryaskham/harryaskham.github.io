# Session summary — snapshot bootstrap timeout

## Goal

Run the caco-web active duty cycle, avoid duplicate work by checking inbox/assigned/ready browser-dashboard beads first, then use lightweight Playwright observation to find and land one focused dashboard trust fix if current evidence warranted it.

## Bead(s)

- `bd-e90e21` — caco-web stays Connected when snapshot bootstrap hangs

## Before state

- Failing tests: none known.
- Relevant metrics: managed caco-web `11180` was healthy but stale at `v1.2.559`; current-assets dev-server observation used `v1.2.562` on a unique temporary port.
- Context: HTTP 5xx and fetch failures were already handled by `bd-c3521a`, but a snapshot request that stayed pending could leave the header green `Connected`, hero `Live SSE connected`, and body stuck at `Snapshot pending` / loading placeholders with no console error.

## After state

- Failing tests: none known.
- Relevant metrics: mocked hanging `/api/v1/ui/snapshot` now aborts after the bounded timeout and renders `Backend unavailable` plus `Dashboard backend unavailable…`; `cargo check -p caco-web --all-targets` and `cargo test -p caco-web --lib` passed.
- Context: snapshot bootstrap uses an `AbortController` timeout and treats `AbortError` / abort-like messages as backend-unavailable so SSE open alone cannot keep the dashboard green while the bulk snapshot is wedged.

## Diff summary

- Commits: `97aa87fdc`
- Files touched: `crates/caco-web/static/app.js`, `crates/caco-web/src/tests.rs`
- Tests: +1 / -0 / flipped 0
- Behavioural delta: a hanging snapshot bootstrap now degrades the operator-facing connection state after a bounded timeout instead of staying indefinitely green and pending.

## Embedded artefacts

- `screenshots/before-snapshot-pending.png` — current-assets dev-server observation showing the pre-fix pending/loading dashboard state.
- `screenshots/after-backend-unavailable.png` — Playwright repro with mocked hanging snapshot showing the post-fix backend-unavailable state.

## Operator-takeaway

The dashboard now distinguishes “SSE opened” from “daemon snapshot is actually usable” even when the failure mode is a hung request rather than an explicit HTTP error, preserving operator trust during daemon/backpressure windows.
