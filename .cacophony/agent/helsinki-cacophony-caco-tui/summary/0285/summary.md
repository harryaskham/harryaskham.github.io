# Session summary — Expose per-scene cache rates in benchmark JSON

## Goal

Continue the caco-tui optimiser loop safely after several speculative render-path experiments regressed under actual Kitty evidence, by improving benchmark observability rather than changing rendering behavior.

## Bead(s)

- `bd-b613a1` — Expose per-scene graphics cache rates in TUI benchmark JSON.
- Related discarded experiments recorded in the optimiser profile: `bd-54a8d6` and `bd-6537e4`.

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: actual Xvfb/kitty real-TUI baseline on current main `918f45517`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈428.8, terminal-inclusive work FPS ≈178.3, avg work ≈2.33ms, avg terminal-inclusive ≈5.61ms, avg upload pass ≈0.70ms. Scenes: `overview_agents` ≈876.9 work FPS / avg ≈1.14ms, `project_beads_board` ≈366.5 / avg ≈2.73ms, `feed_logs` ≈394.5 / avg ≈2.54ms.
- Context: scene summaries already carried raw background/border/decoration cache hit/miss counts, but not derived cache-hit rates, so quick benchmark/perf scripts printed `cache=None` or had to recompute rates by hand.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: a short actual Xvfb/kitty benchmark after the change confirmed JSON now includes per-scene `cache_hit_rate`, `background_cache_hit_rate`, `border_cache_hit_rate`, and `decoration_cache_hit_rate`. Example after-run: `overview_agents` reported all four rates as `1.0`; `project_beads_board` reported aggregate ≈0.99895 with component rates for background, border, and decoration. The after-run had the usual dirty-source caveat because the benchmark binary embeds the last committed SHA before this commit.
- Context: this is observability-only. Rendering, cache behavior, and upload behavior are unchanged.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/app/benchmark_support.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: added `real_benchmark_result_reports_scene_cache_rates_bd_b613a1`; strengthened cached-scene summary assertions for the new fields.
- Behavioural delta: real-TUI benchmark debug scene JSON now exposes derived per-scene cache-hit rates alongside existing hit/miss counts.
- Validation: `./scripts/rustfmt-changed.sh`; `cargo check -p caco-tui`; `cargo test -p caco-tui real_benchmark_result_reports_scene_cache_rates_bd_b613a1`; `cargo test -p caco-tui app::benchmark_support::tests`; actual `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 3 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`; `cargo clippy -p caco-tui --lib -- -D warnings`; full `cargo test -p caco-tui`; `git diff --check`.

## Operator-takeaway

This slice makes future graphics-cache investigations easier and safer: benchmark scene summaries now show cache rates directly, so follow-up work can identify scene-specific cache churn before touching rendering code.
