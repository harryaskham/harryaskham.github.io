# Session summary — streaming kitty native animation frame chunks

## Goal

Continue the active caco-tui optimiser loop using actual Kitty graphics evidence, then apply the proven streaming-base64 upload pattern to terminal-native animation frame uploads.

## Bead(s)

- `bd-ccdd8c` — Stream kitty native animation frame base64 chunks.

## Before state

- Failing tests: none known at the start of the slice.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `9167eec1c`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.5MB, app-side work FPS ≈370.5, terminal-inclusive work FPS ≈168.0, avg work ≈2.70ms, avg terminal-inclusive ≈5.95ms, avg upload pass ≈0.83ms.
- Context: `bd-11b657` had already removed full-payload base64 allocation for placement uploads. The native animation frame emitter still encoded each frame into a full temporary base64 `String` before chunking.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty rerun after streaming native animation frame chunks stayed graphics-gated (`graphics_capability=Kitty`, uploads/deletes observed, terminal-sync enabled): app-side work FPS ≈519.3, terminal-inclusive FPS ≈194.2, avg work ≈1.93ms, avg terminal-inclusive ≈5.15ms, avg upload pass ≈0.61ms. Upload/delete counts and wire bytes stayed effectively the same.
- Context: native animation frame uploads now encode raw payload in 3072-byte chunks that produce the same 4096-character base64 chunks as the previous full-string path.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/kitty.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: no tests added or removed; existing animation-frame byte-equivalence and native-animation command tests cover output preservation.
- Behavioural delta: no intended terminal protocol output change. Native animation frame command bytes remain equivalent to the legacy full-string path; only the internal base64 encoding allocation strategy changes.
- Validation: `cargo test -p caco-tui animation_frame_command_optimization_matches_legacy_byte_for_byte`; `cargo test -p caco-tui animation_stop_command_stops_native_loop`; `cargo test -p caco-tui native_animation_command_loads_frames_and_starts_loop`; `cargo test -p caco-tui kitty::tests::`; `cargo test -p caco-tui`; `git diff --check`; before/after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.

## Operator-takeaway

This slice extends the actual-graphics upload win from placement uploads to terminal-native animation frames. The benchmark remained Kitty/terminal-sync gated and showed lower upload-pass cost plus improved terminal-inclusive FPS without changing emitted protocol bytes.
