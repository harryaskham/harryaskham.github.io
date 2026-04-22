# Session summary 0020 — bd-b69cf3: TUI tmux scrollback display

## Goal

Surface tmux scrollback usage (history-size / history-limit) in the
TUI agent detail view so operators can see how close a long-running
agent is to its 100k-line cap (raised by bd-87f5bf).

## Bead(s)

- `bd-b69cf3` — TUI surface of tmux_history_limit + tmux_history_size.

## Before state

- bd-87f5bf raised the limit and bd-7ef076 exposed the data in
  agents/summary JSON. The TUI had no display.

## After state

- Daemon `AgentSnapshot` (ui_stream.rs) carries
  `tmux_history_limit: Option<u32>` and `tmux_history_size: Option<u32>`.
  Local agents populate both (limit = `AGENT_TMUX_HISTORY_LIMIT`,
  size via `query_tmux_history_size`); remote/queued set None.
- TUI `AgentDisplayState` mirrors the fields.
- Agent detail renders `Scroll: <size>/<limit> lines` after the
  tmux Target row (or `?/<limit>` when size unknown).
- 7 struct-init sites across state/mod.rs, app.rs,
  benchmark_support.rs updated with `None` defaults.

## Diff summary

- Commit: `5d0dd431`.
- Files (5): `crates/caco-daemon/src/ui_stream.rs`,
  `crates/caco-tui/src/state/mod.rs`,
  `crates/caco-tui/src/views/agent_detail.rs`,
  `crates/caco-tui/src/app.rs`,
  `crates/caco-tui/src/app/benchmark_support.rs`.
- `cargo build -p caco-daemon -p caco-tui` + clippy: clean.

## Operator-takeaway

Open the TUI agent detail for any local agent and look at the
`Scroll:` line in the attach metadata section. It shows
`<current_lines>/100000 lines` — when current_lines approaches
100000, the agent's oldest scrollback is being discarded.
