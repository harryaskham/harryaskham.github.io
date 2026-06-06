# Session summary — bd-70b5b2 TUI chat wheel one-message scroll

## Goal

Fix TUI chat mouse-wheel navigation so each wheel up/down event moves by exactly one chat message, avoiding overshooting when reviewing history.

## Bead(s)

- `bd-70b5b2` — Fix TUI chat scroll increment for wheel up/down to single message

## Before state

- Chat keyboard navigation used `chat_scroll_up` / `chat_scroll_down`.
- Mouse wheel up already called `chat_scroll_up` once, but mouse wheel down called `chat_scroll_down`.
- `chat_scroll_down` intentionally collapsed selection into auto-scroll when it reached the newest message; for mouse wheel review, that felt like overshooting because the explicit newest message selection disappeared.

## After state

- Added `TuiState::chat_wheel_scroll_down` for mouse-wheel-specific down navigation.
- Wheel down advances at most one message and keeps the selected message explicit, including when it lands on the newest row.
- Keyboard `j`/Down keeps the existing `chat_scroll_down` behavior, including auto-scroll re-enable at bottom.
- Mouse wheel up remains a single call to `chat_scroll_up`.
- Updated TUI mouse scroll dispatch to call the wheel-specific helper for Chat panes.
- SPEC, README, and AGENTS now document one-message-per-wheel chat navigation.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched:
  - `crates/caco-tui/src/app.rs`
  - `crates/caco-tui/src/state/mod.rs`
  - `crates/caco-tui/src/state/tests.rs`
  - `SPEC.md`
  - `README.md`
  - `AGENTS.md`

## Validation

- `cargo test -p caco-tui chat_wheel_scroll_down_keeps_last_message_selected_bd_70b5b2 --lib` passed.
- `cargo test -p caco-tui chat_scroll --lib` passed.
- `cargo check -p caco-tui --lib` passed.
- `cargo clippy -p caco-tui --lib -- -D warnings` passed.
- `./scripts/rustfmt-changed.sh crates/caco-tui/src/app.rs crates/caco-tui/src/state/mod.rs crates/caco-tui/src/state/tests.rs` formatted the state files and safely skipped pre-existing drift in `app.rs`.

## Operator-takeaway

TUI chat wheel scrolling now has fine-grained one-message movement for both directions, and wheel-down no longer exits explicit selection just because it reaches the newest message.
