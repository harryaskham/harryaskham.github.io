# Session summary — Borrowed log wrap spans

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence and remove avoidable allocation in the log wrapping hot path without changing rendered log text.

## Bead(s)

- `bd-de0c4a` — Borrow fitted log wrap spans.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `122ef5830`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈444.8, terminal-inclusive work FPS ≈182.5, avg work ≈2.25ms, avg terminal-inclusive ≈5.48ms, avg upload pass ≈0.69ms. `overview_agents` was ≈948.6 work FPS / avg ≈1.05ms, `project_beads_board` was ≈383.5 / avg ≈2.61ms, and `feed_logs` was ≈401.9 / avg ≈2.49ms.
- Context: `logs::wrap_spans()` allocated a fresh `String` for every wrapped span chunk, including short timestamp, level, source, and no-search message spans that already borrow from log entries and often fit whole on the current line.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after-run stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). App-side work FPS ≈578.7, terminal-inclusive work FPS ≈201.7, avg work ≈1.73ms, avg terminal-inclusive ≈4.96ms, avg upload pass ≈0.58ms. `overview_agents` measured ≈876.9 work FPS / avg ≈1.14ms, `project_beads_board` ≈535.5 / avg ≈1.87ms, and `feed_logs` ≈516.8 / avg ≈1.94ms.
- Context: whole fitted spans are cloned as existing spans, borrowed span content is sliced without allocating when wrapping, and owned span content still allocates owned chunks so search-highlight/owned paths remain safe.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/views/logs.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: +2 log wrapping tests for borrowed partial chunks and owned partial fallback.
- Behavioural delta: no intended UI change. Log wrapping preserves the existing character-count wrapping semantics, but avoids owned `String` chunks for borrowed fitted/partial span content.
- Validation: `./scripts/rustfmt-changed.sh`; `cargo test -p caco-tui wrap_spans`; `cargo check -p caco-tui`; `cargo test -p caco-tui log_entry_visual_height`; `cargo test -p caco-tui wrap_spans`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4`.

## Operator-takeaway

The log view’s wrapping path now preserves borrowed spans through common no-search rendering instead of allocating identical `String` chunks every frame, with actual Kitty evidence showing feed/log and terminal-inclusive improvement while keeping output stable.
