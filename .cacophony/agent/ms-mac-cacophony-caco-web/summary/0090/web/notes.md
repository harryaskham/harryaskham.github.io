# caco-web duty cycle notes — 0090

- Started from clean checkout synced to `origin/main` at `17eff204aef20414d8352e06d5e6fb3fb0ee3fa2` after bd-05ad06 landed.
- Inbox checked. Noted P0 `bd-9e4be4` coordination, doctor load warnings, and Harry's clarification that macOS is stable and load reporting may be misleading; caco-web stayed scoped to browser-dashboard work.
- Assigned in-progress scan returned no beads for this persistent caco-web agent.
- In-progress caco-web/web scan showed only `bd-1cf76a` owned by ms-dev; ready/open web-adjacent label scans returned no beads.
- Ran current-assets browser observation with `caco-web-observe` against daemon `http://127.0.0.1:11100` via temporary dev server `http://127.0.0.1:63153`.
- Before-fix evidence: global Status hero correctly showed `Snapshot proxy timed out · no usable data returned before the 8s budget`, but Workspace status strip still showed healthy-looking local counts: `0 running`, `0 open / 0 assigned`, `✅ no choices`, and `○ offline` while no usable snapshot data had loaded.
- Dedupe scan found no open/in-progress Workspace duplicate. Filed `bd-bf064a — caco-web Workspace status strip shows zero counts during snapshot timeout` with evidence and then explicitly claimed it because the create-claim output again did not persist ownership immediately.
- Implemented the fix in `crates/caco-web/static/workspace-integrated.js`: when initial snapshot data is unavailable, Workspace status now renders `agents unavailable`, `beads unavailable`, `choices unavailable`, and either `⏱ snapshot timeout` or `○ snapshot delayed` instead of real-looking zero counts/offline copy.
- Added focused static test `workspace_status_bar_avoids_zero_counts_during_snapshot_timeout_bd_bf064a` in `crates/caco-web/src/tests.rs`.
- Validation passed: `cargo fmt --all -- --check`, `CARGO_BUILD_JOBS=2 cargo test -p caco-web workspace_status_bar_avoids_zero_counts_during_snapshot_timeout_bd_bf064a --lib`, `CARGO_BUILD_JOBS=2 cargo test -p caco-web app_js_labels_delayed_200_snapshot_sentinel_bd_05ad06 --lib`, and `CARGO_BUILD_JOBS=2 cargo check -p caco-web --all-targets`.
- After-fix observation via temporary dev server `http://127.0.0.1:63966` confirmed Workspace now shows `agents unavailable beads unavailable choices unavailable ⏱ snapshot timeout` and console stayed clean (`0` errors / `0` warnings).
- Reflection: the recurring `caco bd create --claim true` success/ownership mismatch was already filed as draft `bd-9d60d9` during the previous cycle, so no duplicate reflection bead was filed.
