# Session summary — Direct feed image surface key construction

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence. The session first tested a log-span preallocation idea, discarded it after target-scene regression, then landed a narrower feed-image surface-key allocation cleanup.

## Bead(s)

- `bd-d30fa3` — Preallocate log entry spans (experiment discarded; returned to draft).
- `bd-42cc81` — Build feed image surface keys directly.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `2701aa979`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈384.1, terminal-inclusive work FPS ≈172.0, avg work ≈2.60ms, avg terminal-inclusive ≈5.81ms, avg upload pass ≈0.77ms. `overview_agents` was ≈683.9 work FPS / avg ≈1.46ms, `project_beads_board` was ≈294.1 / avg ≈3.40ms, and `feed_logs` was ≈417.2 / avg ≈2.40ms.
- Context: `bd-d30fa3` attempted to preallocate log entry spans but regressed the target `feed_logs` scene to ≈305.2 work FPS, so that code was reverted and the bead was returned to draft. The landed slice targets `feed_image_surface_key()`, which built each feed image key via `format!("{}{}_{}", current_panel_id_prefix(), FEED_IMAGE, entry_id)`.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after-run stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). App-side work FPS ≈476.5, terminal-inclusive work FPS ≈188.5, avg work ≈2.10ms, avg terminal-inclusive ≈5.31ms, avg upload pass ≈0.64ms. `overview_agents` measured ≈701.4 work FPS / avg ≈1.43ms, `project_beads_board` ≈474.5 / avg ≈2.11ms, and `feed_logs` ≈398.6 / avg ≈2.51ms.
- Context: feed image surface keys now use `String::with_capacity` plus `push_str`/`push` to produce identical bytes without formatting machinery. Overall and terminal-inclusive metrics improved; `feed_logs` was slightly lower/noisy, so this is recorded as an allocation cleanup rather than a feed-scene FPS claim.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/views/feed.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: existing feed image surface-key and feed image fixture tests cover output preservation.
- Behavioural delta: no intended UI change. Feed image surface keys are byte-identical; construction avoids `format!`.
- Validation: `./scripts/rustfmt-changed.sh`; `cargo test -p caco-tui feed_image_surface_key_uses_panel_prefix`; `cargo test -p caco-tui feed_entry_with_image_has_fields`; `cargo check -p caco-tui`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4`.

## Operator-takeaway

The useful landed change is a tiny feed-image allocation cleanup; the more important operational detail is that the log-span preallocation idea was explicitly discarded after target-scene regression, so no suspect log rendering change was carried forward.
