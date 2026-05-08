# Session summary — direct benchmark native animation upload appends

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence, discard a regressing benchmark-buffer experiment, then land one narrow positive benchmark upload-path optimisation.

## Bead(s)

- `bd-c3324c` — Append benchmark native animation uploads directly.
- `bd-bb7377` — Avoid copying benchmark upload batch into combined buffer (discarded back to draft after regression).

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `a2a829946`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈352.1, terminal-inclusive work FPS ≈164.9, avg work ≈2.84ms, avg terminal-inclusive ≈6.06ms, avg upload pass ≈0.83ms.
- Context: first experiment `bd-bb7377` built the benchmark combined upload batch directly, but actual Kitty reruns regressed to ≈303 app-side FPS and ≈151 terminal-inclusive FPS. That code was reverted and the bead returned to draft. The positive slice targeted a smaller copy: `append_benchmark_native_animation_uploads` still called `SurfaceManager::native_animation_command()` to build a temporary `Vec`, then copied it into the native batch.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after run stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). App-side work FPS ≈463.0, terminal-inclusive work FPS ≈186.1, avg work ≈2.16ms, avg terminal-inclusive ≈5.37ms, avg upload pass ≈0.68ms.
- Context: benchmark native animation upload batching now calls `SurfaceManager::append_native_animation_command()` directly into the existing batch and measures wire bytes from the appended delta, preserving existing native upload bookkeeping and phase structure.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/app/benchmark_support.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: no tests added; existing native benchmark upload and benchmark-path tests cover the changed helper.
- Behavioural delta: no user-visible UI change. The real-TUI fixture benchmark path avoids creating and copying a temporary full native-animation command buffer.
- Validation: `cargo check -p caco-tui`; `cargo test -p caco-tui benchmark_native_upload_batch_includes_fresh_native_surface`; `cargo test -p caco-tui benchmark --lib -- --nocapture`; `cargo test -p caco-tui`; `git diff --check`; before/after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.

## Operator-takeaway

The useful lesson is precise: direct appends help when they remove an obvious full command buffer/copy, but broader buffer restructuring can regress. `bd-c3324c` keeps the benchmark upload phases unchanged and only removes the native-animation temporary command allocation.
