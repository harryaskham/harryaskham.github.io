# Session summary — borrowed feed row padding

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence and remove a per-visible-feed-row allocation from the feed/logs scene.

## Bead(s)

- `bd-a30423` — Borrow feed row padding spaces.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `cbf65eb1b`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈400.0, terminal-inclusive work FPS ≈174.9, avg work ≈2.50ms, avg terminal-inclusive ≈5.72ms, avg upload pass ≈0.76ms. Feed/logs scene was slowest: ≈344.2 work FPS, avg ≈2.91ms.
- Context: feed row right-alignment padded the node/project suffix with `" ".repeat(padding)` for each visible feed row that had a suffix, allocating a fresh space string every render.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after-run stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). App-side work FPS ≈469.2, terminal-inclusive work FPS ≈187.0, avg work ≈2.13ms, avg terminal-inclusive ≈5.35ms, avg upload pass ≈0.63ms. Feed/logs scene improved to ≈436.9 work FPS, avg ≈2.29ms.
- Context: feed padding now borrows a slice from a static space buffer for common terminal widths and falls back to allocation only for unusually large padding.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/views/feed.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: added `feed_padding_spaces_borrows_common_widths_bd_a30423` to assert common padding widths borrow and oversized widths still allocate correctly.
- Behavioural delta: no intended UI change. Feed rows retain the same right-aligned node/project suffix layout while avoiding the common padding allocation.
- Validation: `cargo test -p caco-tui feed_padding_spaces_borrows_common_widths_bd_a30423`; `cargo test -p caco-tui borrowed_feed_helpers_preserve_short_rows_bd_a7dff0`; `cargo check -p caco-tui`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4`.

## Operator-takeaway

This removes a small allocation from every visible feed row with a right suffix by reusing static space slices for padding, giving a measurable feed/logs scene improvement in the actual Kitty benchmark.
