# caco-web duty cycle notes — 0089

- Started from clean checkout at `origin/main` `cb18911c6055d2938d6be02f1ac194cb0b39a5bb`.
- Inbox included P0 `bd-9e4be4` coordination and local daemon outage warnings; caco-web stayed scoped to browser-dashboard work and did not duplicate the P0 owner.
- Initial bead scan partially failed during a local daemon/beads outage window; retried after recovery.
- Retry showed no assigned in-progress bead for this persistent caco-web agent and no ready/open `web` or `caco-web` bead. Existing `bd-1cf76a` remained in progress under ms-dev.
- Ran current-assets observation with `caco-web-observe` against daemon `http://127.0.0.1:11100` via temporary dev server `http://127.0.0.1:63294`.
- Before-fix evidence: every major route stayed at connection `Snapshot delayed`; the Status hero correctly avoided zero-count copy but only said `Waiting for daemon snapshot · counts unavailable until backpressure clears`; server logs showed repeated `/api/v1/ui/snapshot` requests returning `200` after ~8001-8003ms and Summaries taking 15164ms; network summary had repeated `/api/v1/node` `net::ERR_ABORTED`; console stayed clean.
- Dedupe scan showed no open ready duplicate. `bd-0e204c` was already closed and covered the healthy-empty-cluster copy; this cycle exposed the narrower follow-up that a completed timeout sentinel still looked like a pending request.
- Filed and explicitly claimed `bd-05ad06 — caco-web stays snapshot delayed after delayed 200 snapshots`. The initial `caco bd create --claim true` output said "created and claimed" but the bead later appeared open/unassigned, so I claimed it explicitly.
- Implemented a focused UI copy fix in `crates/caco-web/static/app.js`: initial empty snapshot-degraded state now says `Snapshot proxy timed out · no usable data returned before the 8s budget`, and the freshness pill says `Snapshot proxy timed out` with a tooltip explaining that the web proxy returned a timeout sentinel before usable data was available.
- Added `app_js_labels_delayed_200_snapshot_sentinel_bd_05ad06` in `crates/caco-web/src/tests.rs`.
- Validation passed: `cargo fmt --all -- --check`, `CARGO_BUILD_JOBS=2 cargo test -p caco-web app_js_labels_delayed_200_snapshot_sentinel_bd_05ad06 --lib`, `CARGO_BUILD_JOBS=2 cargo test -p caco-web app_js_snapshot_delay_avoids_empty_cluster_counts_bd_0e204c --lib`, and `CARGO_BUILD_JOBS=2 cargo check -p caco-web --all-targets`.
- After-fix observation via temporary dev server `http://127.0.0.1:62327` confirmed the new copy appears in Status: `Snapshot proxy timed out · no usable data returned before the 8s budget` and the freshness pill reads `Snapshot proxy timed out`. Console remained clean.
- Reflect-session filed draft `bd-9d60d9` for the `caco bd create --claim true` mismatch: the command reported `created and claimed`, but the bead later appeared open/unassigned until explicitly claimed.
