# Session summary — snapshot-delay hero copy

## Goal

Run the caco-web active duty cycle, file exactly one focused web bead if the browser observation produced evidence, and fix the small operator-trust issue uncovered by the pass.

## Bead(s)

- `bd-0e204c` — caco-web snapshot delay renders dashboard as empty cluster.

## Before state

- Failing tests: none known for caco-web at cycle start.
- Relevant metrics: assigned in-progress scan returned no beads for this agent; ready/open scans for `web`, `caco-web`, `dashboard`, `browser`, `workspace`, `summaries`, `visual-polish`, `terminal`, `interactive`, `agent-interaction`, `notifications`, and `ui` returned no beads.
- Context: current-assets observation showed the dashboard in `Snapshot delayed` / `Snapshot pending` while the status hero summary still rendered healthy-looking zero counts: `0 active agents · 0 open beads · 0 services · 0 recent events`. Dev-server logs showed `/api/v1/ui/snapshot` requests returning 200 sentinel responses after about 8001-8002ms.

## After state

- Failing tests: none observed in focused validation.
- Relevant metrics: `cargo fmt --all`, `cargo test -p caco-web app_js_snapshot_delay_avoids_empty_cluster_counts_bd_0e204c --lib`, `cargo test -p caco-web app_js_labels_snapshot_proxy_timeout_as_degraded_bd_d78de9 --lib`, and `cargo check -p caco-web --all-targets` passed. The after-observation stayed console-clean and showed the new hero summary: `Waiting for daemon snapshot · counts unavailable until backpressure clears`.
- Context: the dashboard no longer presents initial snapshot backpressure as a healthy empty cluster in the high-level status hero. The existing snapshot-degraded pills remain visible.

## Diff summary

- Commits: implementation and recorded-summary commit for `bd-0e204c`.
- Files touched: `crates/caco-web/static/app.js`, `crates/caco-web/src/tests.rs`, and summary artifacts under `.cacophony/agent/ms-mac-cacophony-caco-web/summary/0086/`.
- Tests: +1 focused static contract test for the snapshot-delayed initial-load hero copy.
- Behavioural delta: `renderStatusHero()` now detects the initial degraded-snapshot/no-data state and uses explicit unavailable-counts copy instead of deriving zero-count fragments from empty arrays.

## Embedded artefacts

- `web/board-and-inbox-scan.log` — inbox, assigned-bead, and ready/open web-adjacent bead scan.
- `web/dedupe-scan.log` — open/in-progress/draft caco-web/web dedupe scan before filing `bd-0e204c`.
- `web/filed-bead.log` — `bd-0e204c` creation and claim output.
- `web/observation.log` — before-fix `caco-web-observe` transcript showing snapshot-delayed zero-count hero copy.
- `web/observation-after-fix.log` — after-fix observation transcript showing the new unavailable-counts hero copy.
- `web/server.log` and `web/server-after-fix.log` — temporary current-assets dev-server logs for before/after observation runs.
- `web/notes.md` — concise duty-cycle notes, filing decision, implementation summary, and validation results.
- `web/page-snapshots/*.yml` — Playwright page snapshots for the before/after runs.
- `web/screenshots/*.png` — bounded screenshots captured during the before/after observations across dashboard views.

## Operator-takeaway

The duty cycle found and fixed a small but important trust issue: when the daemon is slow to produce the initial dashboard snapshot, caco-web now says counts are unavailable instead of implying the cluster has zero agents, zero beads, and zero services.
