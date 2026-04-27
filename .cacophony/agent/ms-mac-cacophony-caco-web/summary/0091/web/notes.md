# caco-web duty cycle notes — 0091

- Resumed after a previous scan timed out while the checkout was one commit behind `origin/main` and a transient `.git/index.lock` blocked `caco agent rebase`.
- Inspected the lock: `.git/index.lock` no longer existed, no rebase state was present, and the checkout was simply behind `origin/main`.
- Rebased successfully onto `origin/main` at `75ec9102eb9bb77c8df42b16430a6d88aecb5c33`.
- Inbox checked. Noted repeated direct-recorded hold messages and daemon outage reporting; caco-web continued to avoid `direct,recorded` and stayed scoped to browser-dashboard work.
- Assigned in-progress scan returned no beads for this persistent caco-web agent.
- Ready/open web-adjacent scans found no actionable caco-web/web/workspace/dashboard/browser/summaries/visual-polish/terminal/interactive/agent-interaction/notifications/ui beads.
- In-progress web scan showed only `bd-1cf76a` owned by ms-dev.
- Ran current-assets browser observation with `caco-web-observe` against daemon `http://127.0.0.1:11100` via temporary dev server `http://127.0.0.1:63783`.
- Before-fix evidence: Status and Workspace correctly showed snapshot-timeout/unavailable copy, but Feed still rendered `0 events` plus `No feed events yet · Cluster activity will appear here as it happens` while initial snapshot data was unavailable.
- Dedupe scan found no open/in-progress Feed duplicate. Filed and claimed `bd-2418f5 — caco-web Feed says no events during snapshot timeout`.
- Implemented the fix in `crates/caco-web/static/app.js`: Feed now detects `events.length === 0 && isAwaitingInitialSnapshotData()`, changes the count to `events unavailable`, and renders `Feed events unavailable` / `Feed events delayed` empty states instead of the healthy `No feed events yet` copy while initial snapshot data is unavailable.
- Added focused static test `app_js_feed_empty_state_respects_snapshot_timeout_bd_2418f5` in `crates/caco-web/src/tests.rs`.
- Validation passed: `cargo fmt --all -- --check`, `CARGO_BUILD_JOBS=2 cargo test -p caco-web app_js_feed_empty_state_respects_snapshot_timeout_bd_2418f5 --lib`, `CARGO_BUILD_JOBS=2 cargo test -p caco-web app_js_labels_delayed_200_snapshot_sentinel_bd_05ad06 --lib`, and `CARGO_BUILD_JOBS=2 cargo check -p caco-web --all-targets`.
- After-fix observation via temporary dev server `http://127.0.0.1:62284` confirmed Feed now shows `events unavailable Feed events unavailable The dashboard snapshot timed out before feed data was available. Retrying automatically.` Console remained clean (`0` errors / `0` warnings).
- Reflect-session filed draft `bd-bc0eb0` for the misleading `caco agent rebase` classification that labelled a transient `.git/index.lock` failure as a merge conflict.
