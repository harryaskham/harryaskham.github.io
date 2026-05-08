# Session summary — direct kitty retained-display command writes

## Goal

Continue the active caco-tui optimiser loop with a fresh actual Kitty graphics benchmark, then remove another measured allocation from the terminal graphics command path without changing protocol output.

## Bead(s)

- `bd-870220` — Avoid temporary string for kitty retained display commands.

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `1848f7468`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=228, deletes=237, upload wire bytes ≈26.55MB, app-side work FPS ≈252.5, terminal-inclusive work FPS ≈138.3, avg work ≈3.96ms, avg terminal-inclusive ≈7.23ms, avg upload pass ≈1.29ms. The baseline was host-noisy/slow compared with the previous slice, so comparisons should be treated cautiously.
- Context: `append_retained_display_command_with_placement` still built a `z_clause` `String` plus a formatted command `String` before appending or tmux byte-stuffing. The retained-display command is used by real TUI and benchmark retained image paths.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after run stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). App-side work FPS ≈441.8, terminal-inclusive work FPS ≈182.6, avg work ≈2.26ms, avg terminal-inclusive ≈5.48ms, avg upload pass ≈0.66ms.
- Context: retained display command emission now writes directly into the caller output buffer for non-tmux and uses one small scratch buffer for tmux passthrough. A new byte-preservation test covers z-index, no-z, and tmux-wrapped output.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/kitty.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: added one targeted retained-display command byte-preservation test.
- Behavioural delta: no intended terminal protocol output change. Retained-display command construction avoids short temporary `String` allocations and keeps the legacy bytes exactly.
- Validation: `cargo test -p caco-tui retained_display_command_with_placement_preserves_protocol_bytes_bd_870220`; `cargo test -p caco-tui replacement_display_commands_do_not_delete_previous_placement`; `cargo check -p caco-tui`; `cargo test -p caco-tui kitty::tests::`; `cargo test -p caco-tui`; `git diff --check`; before/after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.

## Operator-takeaway

This is a narrow continuation of the proven kitty command direct-write pattern. It removes another allocation from retained graphics placement while preserving byte-for-byte protocol output and keeping the evidence Kitty/terminal-sync gated.
