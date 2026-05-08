# Session summary — Feed rows reuse ASCII-fast truncation

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence and remove duplicated, slower feed-row truncation logic now that the shared TUI truncation helper has an ASCII fast path.

## Bead(s)

- `bd-7d826a` — Reuse ASCII-fast truncate helper in feed rows.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `eb3a7f1da`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈374.4, terminal-inclusive work FPS ≈169.3, avg work ≈2.67ms, avg terminal-inclusive ≈5.90ms, avg upload pass ≈0.78ms. `project_beads_board` was ≈286.8 work FPS / avg ≈3.49ms; `feed_logs` was ≈392.0 work FPS / avg ≈2.55ms.
- Context: `views/feed.rs` still carried a local `truncate_cow()` implementation that counted Unicode chars before deciding whether to borrow, duplicating the pre-`bd-367224` implementation instead of using `common::truncate_cow()`.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after-run stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). App-side work FPS ≈478.7, terminal-inclusive work FPS ≈186.8, avg work ≈2.09ms, avg terminal-inclusive ≈5.35ms, avg upload pass ≈0.64ms. `project_beads_board` measured ≈462.0 work FPS / avg ≈2.16ms and `feed_logs` measured ≈399.1 work FPS / avg ≈2.51ms.
- Context: feed sender and event-type truncation now route through `common::truncate_cow()`, so common ASCII feed text uses the shared byte-length fast path while retaining Unicode-safe fallback behavior.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/views/feed.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: no new visible-behavior tests were needed; existing feed truncation helper tests now exercise the shared helper and preserve short-row borrowing plus truncation output.
- Behavioural delta: no intended UI change. Feed sender and event-type truncation output remains unchanged, but ASCII values avoid the duplicated Unicode char-count prepass.
- Validation: `cargo test -p caco-tui borrowed_feed_helpers`; `cargo check -p caco-tui`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4`.

## Operator-takeaway

Feed rendering now reuses the shared ASCII-fast truncation helper rather than maintaining a duplicate slower copy; the feed-specific metric moved only modestly in noisy Kitty evidence, but the change removes duplicated hot-path code and preserves all visible truncation behavior.
