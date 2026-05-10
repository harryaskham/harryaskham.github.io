# Session summary — pre-sized replace command buffer

## Goal

Run the active caco-tui optimizer cycle, verify no focused TUI/performance/graphics bead was already assigned, inspect remaining Kitty graphics command-construction hot paths, and land one narrow no-behaviour-change buffer-sizing cleanup.

## Bead(s)

- `bd-dfe7de` — Pre-size Kitty replace command buffers

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: no FPS claim for this micro-change; inspection of `crates/caco-tui/src/kitty.rs` found `SurfaceManager::replace_command` starting from `Vec::new()` before appending a predictable delete command and replacement Kitty placement command.
- Context: adjacent Kitty command helpers had already been moved to pre-sized owned buffers; the replace helper still had an avoidable empty-Vec growth path, especially for retained/empty-data placements where the append helper does not perform the large upload reservation.

## After state

- Failing tests: none observed.
- Relevant metrics: `replace_command` now sizes its owned buffer from a delete-command estimate plus the shared placement-command capacity estimate. The existing cursor-placement capacity helper now reuses that shared placement estimate.
- Context: runtime protocol bytes and tmux wrapping are unchanged. `replace_command_includes_delete_and_place` now also asserts exact plain bytes for the empty-data retained-placement case.

## Diff summary

- Commits: final branch/reintegration commit to be assigned by `caco agent reintegrate`.
- Files touched: `crates/caco-tui/src/kitty.rs`.
- Tests: strengthened `replace_command_includes_delete_and_place`; no tests removed.
- Behavioural delta: no visible change; the replace command builder now avoids starting from an empty `Vec` and has exact-byte coverage for delete-plus-retained-placement output.
- Validation: `./scripts/rustfmt-changed.sh`; `git diff --check`; queued `cargo test -p caco-tui replace_command_includes_delete_and_place` (`tj-46f79f25`); queued `cargo check -p caco-tui` (`tj-a7a6bc18`); queued `cargo clippy -p caco-tui --lib -- -D warnings` (`tj-3c22c2ff`); queued `cargo test -p caco-tui` (`tj-e9f0fcff`).

## Operator-takeaway

The Kitty replace-command helper now follows the same predictable-capacity pattern as the adjacent graphics command builders, preserving exact protocol bytes while trimming another small allocation-growth path in command construction.
