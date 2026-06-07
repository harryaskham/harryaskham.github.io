# bd-a1b00a Shift+I broadcast per-pane resize

## Work
- Fixed Shift+I broadcast attach sizing so each visible attached agent pane carries the PTY/tmux winsize computed from that pane's own rendered container.
- Added `VisibleAttachableAgentPane` collection for broadcast attach, including main workspace panes and bottom-panel panes.
- Routed broadcast attach and delayed attach-metadata completion through the per-pane resize data instead of falling back to the focused/primary pane size.
- Factored embed/PTY resize calculations so bottom-panel panes can compute winsizes from their own content area.
- Added regression `shift_i_broadcast_uses_each_visible_pane_resize_bd_a1b00a` proving differently sized panes get independent resize cache entries.

## Validation
- `./scripts/rustfmt-changed.sh --check crates/caco-tui/src/app.rs`
- `cargo test -p caco-tui shift_i_broadcast_uses_each_visible_pane_resize_bd_a1b00a --lib`
- `cargo test -p caco-tui shift_i_includes_bottom_panel_visible_agent_bd_shifti_bottom --lib`
- `cargo check -p caco-tui --lib`
- `cargo clippy -p caco-tui --lib -- -D warnings`
- `git diff --check`

## Notes
- Documentation contract: no operator workflow or CLI/API surface changed; this is an internal TUI broadcast attach bug fix.
