# Session summary — Shift+I attached-pane styling

## Goal

Make the TUI's Shift+I broadcast attachment mode visibly attach all rendered agent panes, instead of making only the currently active agent pane look attached while the rest appear preview-only.

## Bead(s)

- `bd-5ae6b8` — Fix Shift+I TUI agent pane attachment focus and styling.

## Before state

- In Shift+I broadcast mode, `tmux_broadcast` routed input to all visible attachable panes, but `render_tmux_embed` treated only `attached_pane_id` as the strong attached styling signal.
- Broadcast mode intentionally clears `attached_pane_id`, so each visible pane could render with preview styling even while broadcast input mode was active.
- Existing styling hooks already had a stronger Active state for agent terminal subpanels, but broadcast attachment did not use it.

## After state

- `render_tmux_embed` now treats `tmux_attached && tmux_broadcast` as an attached styling state for every rendered visible agent terminal pane.
- Single-pane attachment still uses `attached_pane_id` as the authoritative input target.
- Broadcast-attached panes now record the same Active/green/glowing graphics border affordance as single attached panes.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched:
  - `crates/caco-tui/src/views/agent_detail.rs`
  - `.cacophony/agent/winmini-cacophony-caco-dev-wmi-3/summary/pending/summary.md`
- Tests/validation:
  - `cargo test -p caco-tui broadcast_attached_agent_terminal_records_active_green_border_bd_5ae6b8 --lib`
  - `cargo test -p caco-tui attached_agent_terminal_records_active_green_border_bd_4b6295_bd_320fe5 --lib`
  - `cargo check -p caco-tui --lib`
  - `cargo clippy -p caco-tui --lib -- -D warnings`
  - `./scripts/rustfmt-changed.sh crates/caco-tui/src/views/agent_detail.rs`
  - `git diff --check`
- Behavioural delta: Shift+I broadcast mode now visually communicates that all visible agent terminal panes are attached/active, while preserving single-pane input-target semantics.

## Operator-takeaway

The confusing state where Shift+I made panes flicker but only the active pane looked truly attached should be resolved: broadcast-attached agent panes now share the strong active styling instead of preview styling.
