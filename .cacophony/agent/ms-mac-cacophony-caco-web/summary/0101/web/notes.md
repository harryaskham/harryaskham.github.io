# caco-web duty cycle notes — 0101

- Rebased to current `origin/main` at the start of the cycle.
- Inbox contained routine Android/TUI/macOS/log-monitor updates and broken-on-main chatter already owned by other agents.
- Assigned-bead scan found no in-progress bead for `ms-mac-cacophony-caco-web`.
- Ready/open web-adjacent scans found no available `caco-web`, `web`, `workspace`, `dashboard`, `browser`, `summaries`, `visual-polish`, `terminal`, `interactive`, `agent-interaction`, `notifications`, `ui`, `feed`, `operator-trust`, or `tests` bead.
- Initial current-assets `caco-web-observe` found a fresh Summaries defect: selecting `ms-mac/caco-android #0013` produced two browser console 404s for `/api/v1/summaries/ms-mac-cacophony-caco-android/13/raw/.cacophony/agent/ms-mac-cacophony-caco-android/summary/0013/screenshots/android-bd29ebd0-seed-verify.png?project=cacophony`.
- Filed and claimed `bd-7c5d34 — caco-web summary images 404 when markdown uses recorded-summary paths`.
- Implemented `normalizeSummaryArtefactPath(item, relPath)` in `crates/caco-web/static/summaries.js`, stripping `.cacophony/agent/<agent>/summary/<index>/` and related summary prefixes before constructing raw artifact URLs.
- Added static regression `summaries_raw_urls_normalize_recorded_summary_paths_bd_7c5d34` in `crates/caco-web/src/tests.rs`.
- Validation passed: `cargo fmt --all -- --check`, targeted `bd-7c5d34` test, existing screenshot preview regression, and `cargo check -p caco-web --all-targets`.
- After-fix `caco-web-observe` confirmed console clean (`0` errors / `0` warnings`) and raw screenshot URL normalized to `/raw/screenshots/android-bd29ebd0-seed-verify.png` with `200 OK` responses.
- Reflection filed draft `bd-1ea164 — Align caco bd list status filter docs with CLI behavior` because `caco bd list --status open,in_progress` failed despite repository guidance implying comma-separated status filters.
