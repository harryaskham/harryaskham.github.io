# Session summary — workspace-view log tail pane (bd-b9e32e)

## Goal

Land a workspace-view-ready log tail pane: source-polymorphic SSE
streaming endpoint on the daemon, plus a reusable browser-side pane
module that filters / pauses / follows / copies on the client. Built
to the contracts published in bd-a78749 (MVP) so the pane can be
mounted by the workspace shell once MVP lands without a serial rebase.

## Bead(s)

- `bd-b9e32e` — [workspace-view] Log tail pane: agent/daemon/session
  logs with grep filter + follow-mode + pause
- (parent: `bd-027e9d` — caco-web Workspace View epic)

## Before state

- Daemon had `/api/v1/logs/stream` for the daemon log only — no
  source-polymorphic surface, no agent/session-scoped tailing.
- caco-web had no log-pane module; the agent-detail surface read
  one-shot log dumps via `handle_agent_logs_endpoint`, no streaming.
- No client-side regex filter, pause, or 10k-line ring buffer in
  any caco-web surface.

## After state

- New daemon endpoint `GET /api/v1/logs/tail/{source}[/{id}]` with
  query params `tail` (1-5000, default 200) and `follow` (default
  true). Sources: `daemon` | `agent` | `session`.
- `LogTailStrategy` enum cleanly separates file-tail (daemon log,
  wrapper log) from tmux-pane capture (agent live output) with a
  shared snapshot/diff abstraction.
- `escape_sse_line` collapses CR/LF so a source line is always
  exactly one SSE `data:` frame.
- `diff_tmux_new_lines` handles both append-only (prefix strip) and
  rolled-buffer (set-diff fallback) tmux capture cases.
- New static module `static/workspace-log-pane.js`:
  - `window.WorkspaceLogPane.mount(container, config) → handle`
  - regex filter, pause/resume, follow toggle (auto-unset on
    scroll-up), copy-visible button, 10k-line client buffer
  - auto-reconnect with exponential-ish backoff (2s) on EventSource
    error
  - `applyFilter(lines, regex)` pure helper exposed for downstream
    panes that want to validate filter behaviour without spinning up
    a real EventSource
- New `static/workspace-log-pane.css` with toolbar/body layout.
- 7 daemon tests + 3 caco-web tests, all passing:
  - `workspace_log_tail_daemon_returns_sse_frames`
  - `workspace_log_tail_unknown_source_returns_400`
  - `workspace_log_tail_agent_without_id_returns_400`
  - `workspace_log_tail_agent_unknown_id_returns_404`
  - `escape_sse_line_collapses_newlines_and_carriage_returns`
  - `diff_tmux_new_lines_returns_prefix_stripped_tail`
  - `diff_tmux_new_lines_handles_rolled_buffer`
  - `workspace_log_pane_js_is_embedded`
  - `workspace_log_pane_css_is_embedded`
  - `workspace_log_pane_filter_semantics_mirror_bead_acceptance`
    (AC #7: 1000 lines, /error/ filter, only error lines visible)

## Diff summary

- Files touched:
  - `crates/caco-daemon/src/lib.rs` — 2 routes + handlers + helpers + 7 tests
  - `crates/caco-web/static/workspace-log-pane.js` — new
  - `crates/caco-web/static/workspace-log-pane.css` — new
  - `crates/caco-web/src/tests.rs` — 3 new tests
- Tests: +10 / -0 / flipped 0
- Behavioural delta: workspace shell can now mount a log pane against
  any agent / the daemon log / a session log via one-line JS, with
  full-stack live tailing through a documented SSE contract.

## Operator-takeaway

The pane is a self-contained module — workspace MVP can drop in a
`<div id="logpane"></div>` and call
`WorkspaceLogPane.mount(el, {source:'agent', agent_id:'…'})` to get a
production-ready filtered live tail. The only daemon contribution is
the SSE endpoint; everything operator-facing (filter, pause, follow,
copy) is client-side so it can be improved without daemon redeploy.
