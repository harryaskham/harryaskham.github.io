# bd-2fa0b2 — caco-tui agent-detail lazy-fetch: full goal + reintegration_history from GET /agents/{id}

## Bead
bd-2fa0b2 (caco-tui/client/serve-resilience/snapshot, P1; TUI slice of bd-1ab041, the client lazy-fetch fast-follow for the bd-44e811 P0 ui/snapshot debloat). msm-3 landed bd-44e811 daemon-first: the ui/snapshot now carries a TRUNCATED agent.goal (~500 chars) + EMPTY reintegration_history; the FULL values remain available via the existing daemon GET /api/v1/agents/{id} (handle_agent_status_show serializes full AgentInfo — no new endpoint). caco-tui agent-detail rendered both from the snapshot-derived AgentDisplayState, so on quiet nodes it showed truncated goal + empty history until this lands. iOS needs nothing; web + macOS are separate bd-1ab041 owner slices. msm-3 explicitly declined the TUI slice (caco-tui specialist surface) → taken by this caco-tui-capable dev worker.

## Implementation (mirrors the existing /diff lazy-fetch plumbing)
- **client.rs**: `AgentFullDetail { goal, reintegration_history }` (deserializes only the two heavy fields; serde ignores the rest of AgentInfo) + `fetch_agent_detail_full(id)` → GET /api/v1/agents/{id} via `SuccessEnvelope<AgentFullDetail>`. No daemon change.
- **state/mod.rs**: `agent_full_detail: HashMap<String, client::AgentFullDetail>` + `agent_full_detail_fetching: HashSet<String>` (mirrors agent_diff_data/agent_diff_fetching), initialized in the TuiState constructor.
- **event.rs**: `ActionResult::AgentDetailFullFetched { agent_id, detail }` + `AgentDetailFullFailed { agent_id, error }` variants + manual-Debug arms.
- **app.rs**: `request_agent_detail_full(id)` (fetch-once gate: skip if in-flight or cached; spawns the fetch, sends the ActionResult) called alongside `request_agent_diff` at the two focused agent-detail sites (so it only fires for the focused/open detail, never background tabs); result handlers store the detail / cache an empty on failure (avoids re-fetch storm; render falls back per-field).
- **views/agent_detail.rs**: `render_agent_info` computes a single `reint_history` (full detail when non-empty, else snapshot) reused at both `push_reintegration_history_lines` call sites (refactored to take a `history` slice param), and an `effective_goal` (full detail when present, else snapshot) for the Goal section. List/preview label views keep using the snapshot's truncated goal. Per-field graceful fallback during the fetch window; no crash.

## Validation (daemon test queue, --cwd at checkout)
- `cargo clippy -p caco-tui --lib` (tj-663bb894): PASSED (exit 0) — compiles + lints clean across all five files (incl. the `<'a>`-tied history slice + the borrow of state-vs-snapshot history).
- `cargo test -p caco-tui --lib bd_2fa0b2` (tj-c97e3148): PASSED, 2/2 — render_agent_detail_prefers_lazy_fetched_goal_and_history (snapshot EMPTY history + TRUNCATED goal but lazy-fetched full detail → renders the full landed-commit hash + full goal body) + render_agent_detail_falls_back_to_snapshot_when_no_lazy_detail (no lazy detail → snapshot goal + history, graceful). (First run errored as retryable daemon_restart_recovered from an unrelated `caco update --restart`; re-ran clean.)
- rustfmt-clean on changed regions; `git diff --check` clean.
- Headless ratatui TestBackend render tests cover the render + fallback logic; live interactive TUI render confirmation is operator-validated (headless cannot drive the interactive TUI).

## Scope
TUI slice only. iOS confirmed no-op; web (caco-web) + macOS (caco-macos) agent-detail lazy-fetch remain their specialist slices on bd-1ab041.

## Diff
See the reintegration receipt for the landed squash SHA.
