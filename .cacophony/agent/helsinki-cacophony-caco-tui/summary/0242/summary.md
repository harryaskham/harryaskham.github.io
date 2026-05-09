# Session summary — Preallocate bead label chip spans

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence and reduce per-visible-row allocation growth in bead label chip rendering without changing table output.

## Bead(s)

- `bd-814e25` — Preallocate bead label span chips.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `a29746501`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈390.1, terminal-inclusive work FPS ≈173.3, avg work ≈2.56ms, avg terminal-inclusive ≈5.77ms, avg upload pass ≈0.76ms. `overview_agents` was ≈835.2 work FPS / avg ≈1.20ms, `project_beads_board` was ≈332.0 / avg ≈3.01ms, and `feed_logs` was ≈360.1 / avg ≈2.78ms.
- Context: `common::bead_label_spans()` built each visible bead row’s compact label chip spans from `Vec::new()`, even though common rows expand one to three labels into a predictable handful of separator/chip/overflow spans.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after-run stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). App-side work FPS ≈581.1, terminal-inclusive work FPS ≈201.3, avg work ≈1.72ms, avg terminal-inclusive ≈4.97ms, avg upload pass ≈0.57ms. `overview_agents` measured ≈941.7 work FPS / avg ≈1.06ms, `project_beads_board` ≈540.7 / avg ≈1.85ms, and `feed_logs` ≈501.3 / avg ≈1.99ms.
- Context: label span vectors now preallocate a capped expected chip span budget after the existing empty/narrow fast returns. Rendered chip text and overflow behaviour are unchanged.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/views/common.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: +1 bead label span test asserting output is unchanged and capacity is preallocated.
- Behavioural delta: no intended UI change. Bead label chips render the same text, with fewer small vector growth reallocations.
- Validation: `./scripts/rustfmt-changed.sh` was attempted but skipped `common.rs` because the HEAD version is not rustfmt-clean; avoided unrelated formatting churn. Passed `cargo test -p caco-tui bead_label_spans_preallocate_without_render_change_bd_814e25`; `cargo test -p caco-tui bead_label`; `cargo check -p caco-tui`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4`.

## Operator-takeaway

Bead label chip rendering now preallocates the small span vector it predictably needs for visible rows. It is a tiny allocation cleanup, but actual Kitty evidence improved terminal-inclusive and all scene metrics against the fresh baseline.
