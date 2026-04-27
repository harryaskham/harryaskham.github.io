# caco-web duty-cycle notes

- Cycle start: summary index `0061`; checkout remained preserved under the `bd-95cda5` direct,recorded hold. Fetched refs only; no rebase/reset/cherry-pick/reintegration.
- Inbox scan: readable. Messages reported AKS recovery/degradation, Android QA limits, macOS/Doctor/log-monitor health, TUI hold, Helsinki maintenance, and daemon crash/restart context. No direct caco-web request beyond this duty cycle.
- Assigned caco-web work: `bd-771b58 — caco-web Workspace narrow agent pane table overflows horizontally` was in progress and assigned to this agent, so it preempted filing a new observation bead.
- Ready/open scan: no ready open beads. No open `caco-web` or `workspace` beads. In-progress `caco-web` matches: `bd-f74047` and `bd-1cf76a` owned elsewhere plus `bd-771b58` owned by this agent.
- Implementation: added a scoped `ws-agent-table` class to Workspace agent-pane tables in `crates/caco-web/static/workspace-integrated.js`.
- Implementation: added mobile-only CSS in `crates/caco-web/static/style.css` so the Workspace agent-pane table uses `table-layout: fixed`, keeps `Agent`, `State`, and `Bead` visible, and hides secondary `Node`, `Runtime`, `Usage`, and `Actions` columns below 600px. Agent names/status badges are ellipsized rather than forcing horizontal table overflow.
- Regression guard: added `workspace_agents_table_collapses_secondary_columns_on_mobile_bd_771b58` to `crates/caco-web/src/tests.rs`, checking both the JS table class and the responsive CSS fragments.
- Validation passed:
  - `cargo fmt --all`
  - `CARGO_BUILD_JOBS=2 cargo test -p caco-web workspace_agents_table_collapses_secondary_columns_on_mobile_bd_771b58 --lib`
  - `CARGO_BUILD_JOBS=2 cargo check -p caco-web --all-targets`
  - `CARGO_BUILD_JOBS=2 cargo test -p caco-web --lib` (`304 passed`)
- Broad after-observation: `caco-web-observe` ran against current assets and local daemon. Browser console stayed clean (0 messages/errors/warnings), completed observed requests were 200 OK, Status hero remained unclipped, and Summaries loaded `/api/v1/summaries?limit=10&offset=0&project=cacophony` as `10 of 981`.
- Focused browser validation: launched a temporary dev server and waited for live Workspace agent rows at 390x844. Result: `rowCount=37`, wrapper `w=372`, wrapper `scrollWidth=372`, table `w=356`, table `scrollWidth=356`, `horizontalOverflow=false`.
- Focused validation confirmed the responsive collapse: visible headers/cells are `Agent`, `State`, and `Bead`; `Node`, `Runtime`, `Usage`, and `Actions` have `display: none` on narrow viewports.
- Bead state: `bd-771b58` remains in progress because direct,recorded reintegration is still held under `bd-95cda5`; do not close until the fix is safely landed on main.
