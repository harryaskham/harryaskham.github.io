# caco-web duty cycle notes — 0086

- Inbox checked; no operator/controller message assigned new caco-web implementation work.
- Assigned in-progress scan returned no beads for this agent.
- Ready/open web-adjacent label scans returned no beads for: web, caco-web, dashboard, browser, workspace, summaries, visual-polish, terminal, interactive, agent-interaction, notifications, ui.
- Ran current-assets browser observation with `caco-web-observe` against daemon `http://127.0.0.1:11100` via temporary dev server `http://127.0.0.1:62315`.
- Observation produced fresh actionable UI evidence: initial snapshot proxy calls took ~8001-8002ms and the status hero rendered `0 active agents · 0 open beads · 0 services · 0 recent events` while connection state showed `Snapshot delayed` / `Snapshot pending`.
- Dedupe scan found no open ready web/caco-web bead for this issue; `bd-1cf76a` is related observe-helper work owned elsewhere, not this product UI defect.
- Filed and claimed `bd-0e204c — caco-web snapshot delay renders dashboard as empty cluster`.
- Implemented a focused hero-copy fix in `crates/caco-web/static/app.js`: when initial snapshot data is unavailable because snapshot loading is degraded, the hero summary now says `Waiting for daemon snapshot · counts unavailable until backpressure clears` instead of rendering healthy-looking zero counts.
- Added static contract test `app_js_snapshot_delay_avoids_empty_cluster_counts_bd_0e204c`.
- Validation passed: `cargo fmt --all`; `CARGO_BUILD_JOBS=2 cargo test -p caco-web app_js_snapshot_delay_avoids_empty_cluster_counts_bd_0e204c --lib`; `CARGO_BUILD_JOBS=2 cargo test -p caco-web app_js_labels_snapshot_proxy_timeout_as_degraded_bd_d78de9 --lib`; `CARGO_BUILD_JOBS=2 cargo check -p caco-web --all-targets`.
- Reran `caco-web-observe` after the fix; the status view now shows `Waiting for daemon snapshot · counts unavailable until backpressure clears`, console remains clean, and network requests were 200 OK in the after pass.
