# Session summary — in-place relative-age suffix

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence and remove a second string allocation from compact relative-age formatting used in bead and feed rows.

## Bead(s)

- `bd-4f1227` — Avoid second relative-age string allocation.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `993565680`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈477.8, terminal-inclusive work FPS ≈187.4, avg work ≈2.09ms, avg terminal-inclusive ≈5.34ms, avg upload pass ≈0.65ms. Project bead-board scene: ≈386.1 work FPS, avg ≈2.59ms; feed/logs scene: ≈465.4 work FPS, avg ≈2.15ms.
- Context: `common::human_staleness_ago_at()` called `human_staleness_at()` to allocate the compact age string, then used `format!("{s} ago")` to allocate another string for every non-now visible age.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after-run stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). App-side work FPS ≈495.7, terminal-inclusive work FPS ≈191.0, avg work ≈2.02ms, avg terminal-inclusive ≈5.24ms, avg upload pass ≈0.64ms. Project bead-board improved to ≈451.1 work FPS, avg ≈2.22ms; feed/logs was lower/noisy at ≈433.5 work FPS, avg ≈2.31ms.
- Context: `human_staleness_ago_at()` now appends `" ago"` to the existing compact age string in place and still returns `"now"` unchanged for now/future timestamps.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/views/common.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: no new test was required; existing supplied-now staleness tests and feed timestamp tests cover exact output including `"now"` and non-now `"ago"` suffixes.
- Behavioural delta: no intended UI change. Relative-age text remains identical while avoiding the second allocation for non-now ages.
- Validation: `cargo test -p caco-tui staleness_ago_at_uses_supplied_now_bd_0f4848`; `cargo test -p caco-tui views::feed::tests::feed_human_staleness_uses_supplied_now_bd_bb15c4`; `cargo check -p caco-tui`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4`.

## Operator-takeaway

Relative-age formatting now reuses the already-allocated compact age string and appends the suffix in place, reducing per-row churn in both bead updated-age cells and feed timestamps without changing visible text.
