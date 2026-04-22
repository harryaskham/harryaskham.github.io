# Session summary — bd-274c2d cycle: 95-error caco-tui broken-on-main sweep

## Goal

`cargo clippy --workspace --all-targets -- -D warnings` was red on main with ~95 errors. Root cause: msm-5's bd-b69cf3 added two fields (`tmux_history_limit`, `tmux_history_size`) to `AgentDisplayState` and to `caco_daemon::ui_stream::AgentSnapshot`, but only swept some construction sites. Worse, msm-5's commit also incorrectly added the same fields to `AttachMetadata` and `SessionKickedModal` literals — those structs don't have those fields. Took the whole sweep as a bd-274c2d cycle since the workspace was blocking everyone.

## Bead(s)

- `bd-274c2d` — Permanent: continuous test suite health (one cycle).

## Before state

- `cargo clippy --workspace --all-targets -- -D warnings`: 95 errors.
  - Most: `E0063 missing fields tmux_history_limit and tmux_history_size in initializer of state::AgentDisplayState` across `caco-tui` and one site each of `caco_daemon::ui_stream::AgentSnapshot`.
  - Several: `E0560 struct AttachMetadata has no field named tmux_history_limit/size` (also `SessionKickedModal`) — msm-5 incorrectly added these field initializers in `app.rs` constructions.
- `cargo build` was clean (these are clippy/test-target-only errors).

## After state

- `cargo clippy --workspace --all-targets -- -D warnings`: clean.
- `cargo test-small`: 56 pass.
- `cargo test -p caco-tui --lib`: 2817 pass.
- `cargo test -p caco-daemon --lib`: pre-existing stack-overflow in `tests::discover_available_profile_names_includes_checked_in_canonical_profiles_without_checkouts` (msm-2's bd-31b6be territory; not introduced by this cycle and unrelated to the field-sweep work).

## Implementation

- Added `tmux_history_limit: None,` and `tmux_history_size: None,` to every `AgentDisplayState { ... }` and `AgentSnapshot { ... }` struct-literal site that didn't already initialise them. Did this with a brace-counting Python pass that:
  - Skipped string and comment content.
  - Resolved each `<Type> {` to its matching `}` by tracking depth.
  - Refused to insert when the close brace wasn't first non-whitespace on its line (avoids wedging into single-line constructions and `..base` syntax).
  - Refused to touch literals using `..something` base-struct syntax.
  - Did NOT touch `pub struct <Type> { ... }` definitions or `impl <Type> { ... }` blocks.
- For `AttachMetadata` and `SessionKickedModal`, removed the spurious `tmux_history_limit/size: None,` lines that msm-5's commit incorrectly added.
- Added `#[derive(..., Default)]` to `AgentDisplayState` so future sweeps can land cleanly via `..AgentDisplayState::default()` — small struct-shape ergonomics that no future bd-274c2d cycle has to babysit.

## Diff summary

- `crates/caco-tui/src/state/mod.rs` — `Default` derive + 1 literal updated (+5 / -1).
- `crates/caco-tui/src/state/tests.rs` — 63 literals updated (+126 / -0).
- `crates/caco-tui/src/shell_cwd.rs`, `shell_tile_lane.rs`, `views/{agent_detail,chat,fuzzy_picker,project_tree}.rs` — 9 literals updated total.
- `crates/caco-tui/src/app.rs` — 10 spurious `AttachMetadata` / `SessionKickedModal` field lines removed.
- `crates/caco-daemon/src/ui_stream.rs` — 5 literals updated.
- Commit: `<TBD>`.

## Operator-takeaway

This is the third broken-on-main sweep this session driven by struct-field additions that didn't sweep all construction sites. The pattern is consistent and the merge-queue daemon (bd-2c399b) gating clippy pre-merge would have caught all three in seconds. Adding `Default` to `AgentDisplayState` makes the next analogous addition a one-line struct-definition change with `..Default::default()` everywhere else — same ergonomic improvement worth landing on the other big TUI/daemon structs (`AttachMetadata`, `AgentSnapshot`, `SessionKickedModal`, `BeadDisplayState`, etc) opportunistically.
