# bd-ceb32a — Fix shift-I keyboard binding in TUI to attach to all visible agents

## Bead
bd-ceb32a (TUI/agents/keyboard-bindings, P2) — operator-filed (Harry): "pressing shift-I focuses the sidebar instead of attaching keyboard inputs to all visible agents."

## Root cause (po4-1's most-likely #1, host-verifiable)
The standard `Char('I')` Shift+I broadcast arms (`handle_nav_key`, `handle_content_key`) are already correct (`attach_tmux_broadcast()`), so the standard encoding works. But some terminals / the kitty keyboard protocol report **Shift+I as `Char('i') + SHIFT`** rather than `Char('I')`. That misses the `Char('I')` arms and instead hits the plain lowercase `Char('i')` arms:
- nav (`handle_nav_key`): attaches only the **selected** agent (not all visible).
- content (`handle_content_key`): attaches the focused agent-detail / cycles speech routing.
Either way Shift+I does the wrong thing instead of broadcasting to all visible panes.

## Change (crates/caco-tui/src/app.rs)
Dual-encoding fix mirroring the existing Shift+7 / `Char('7')+SHIFT` pattern: added a `Char('i') if key.modifiers.contains(SHIFT) && !self.input_attachment_active()` broadcast arm **before** the plain `Char('i')` arm in both dispatch paths:
- `handle_nav_key`: un-focuses the sidebar (`nav.nav_focused = false`) then `attach_tmux_broadcast()` — identical to the `Char('I')` nav arm.
- `handle_content_key`: `attach_tmux_broadcast()`, placed before the prune-view `Char(ch @ …)` intercept and the plain `Char('i')` arm so the shift case is never swallowed — matching the uppercase `Char('I')` arm's reach.
Both keep the `!input_attachment_active()` guard so an attached pane still owns keyboard passthrough (bd-3f888f). The standard `Char('I')` arms are unchanged, so both encodings now broadcast.

## Tests
`shift_lowercase_i_enters_broadcast_mode_bd_ceb32a`: drives `KeyEvent::new(Char('i'), SHIFT)` through `handle_key` in both content-focused and nav-focused contexts (mirroring the existing `shift_i_enters_broadcast_mode`) and asserts `tmux_attached`/`tmux_broadcast` (content) and sidebar-unfocus + `tmux_broadcast` (nav). The existing `shifted_printables_passthrough_when_{tmux,ssh}_attached_bd_3f888f` tests still pass, confirming the attached-passthrough guard is preserved.

## Validation (daemon test queue, --cwd at checkout)
- `cargo test -p caco-tui --lib shift` (tj-1fe9bbfb): PASSED 25/25 (new + all existing Shift+I tests).
- `cargo clippy -p caco-tui --lib` (tj-59c96478): PASSED, 0 warnings.
- app.rs inserted regions rustfmt-clean (skip_children reformat diff; pre-existing drift untouched); `git diff --check` clean.

## Scope / live-confirm note
This fixes po4-1's most-likely root cause (#1, terminal lowercase-i+SHIFT encoding) and is unit-verified. If the operator's symptom persists on their terminal, it would be the subtler #2 path (an `input_attachment_active()`-true edge or a view-specific focus arm), which needs a live-TUI KeyEvent capture (key.code + modifiers + focus context) that a headless winmini worker can't reproduce — reopen with that capture if so. Related operator bug bd-0e0462 (Tools > Console excluded from shift-I attach-to-all) is a separate PTY-input-routing surface left for a live-TUI/operator pass.

## Diff
See the reintegration receipt for the landed squash SHA.
