# Session summary — Cache feed sender parse context

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence and reduce repeated sender parsing/truncation work in the feed/log-heavy benchmark scene.

## Bead(s)

- `bd-bcbd4b` — Cache feed sender parse context.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `a45a27478`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈349.4, terminal-inclusive work FPS ≈161.6, avg work ≈2.86ms, avg terminal-inclusive ≈6.19ms, avg upload pass ≈0.89ms. `overview_agents` was ≈779.2 work FPS / avg ≈1.28ms, `project_beads_board` was ≈322.7 / avg ≈3.10ms, and `feed_logs` was ≈285.1 / avg ≈3.51ms.
- Context: feed rows already cached consecutive sender colors, but still reparsed the same sender string with `splitn` and recomputed the truncated sender id for every visible row. The benchmark/common feed stream often has repeated consecutive senders.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after-run stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). App-side work FPS ≈505.7, terminal-inclusive work FPS ≈190.8, avg work ≈1.98ms, avg terminal-inclusive ≈5.24ms, avg upload pass ≈0.62ms. `overview_agents` measured ≈813.4 work FPS / avg ≈1.23ms, `project_beads_board` ≈480.3 / avg ≈2.08ms, and `feed_logs` ≈428.6 / avg ≈2.33ms. The after-run emitted the usual dirty-source warning because the benchmark embeds the last git commit, but it rebuilt without `--no-build`.
- Context: feed rendering now has a compact consecutive-sender context cache for parsed machine/id and truncated sender display text, alongside the existing sender style cache. Output and right-suffix semantics are preserved.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/views/feed.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: +1 focused compatibility/cache test for consecutive sender context reuse.
- Behavioural delta: no intended UI/layout change; repeated sender rows reuse parsed/truncated context while preserving sender color cache semantics and right-aligned node/project suffix output.
- Validation: `./scripts/rustfmt-changed.sh`; `cargo check -p caco-tui`; `cargo test -p caco-tui feed_sender_context_cache_reuses_consecutive_sender_bd_bcbd4b`; `cargo test -p caco-tui views::feed::tests`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4`.

## Operator-takeaway

This slice reduces repeated feed-row sender parsing/truncation and produced positive actual Kitty evidence across the target feed_logs scene, all scenes, app-side FPS, and terminal-inclusive FPS. The baseline was noisy/low, so the exact magnitude should be treated cautiously, but the direction is clean for the targeted hot path.
