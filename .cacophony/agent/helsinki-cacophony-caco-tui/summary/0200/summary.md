# Session summary — streaming kitty placement base64 chunks

## Goal

Continue the active caco-tui optimiser loop using actual graphics-gated evidence, identify a real Kitty upload hot path, and land one focused optimisation that improves terminal-inclusive graphics benchmark results without changing kitty command output.

## Bead(s)

- `bd-11b657` — Stream kitty placement base64 chunks without full-payload allocation.
- `bd-82139f` — [broken-on-main] caco-tui TestJob fixtures missing retry fields.
- Discarded experiment context: `bd-a1717a` — Avoid z-clause string allocation in kitty placement commands.

## Before state

- Failing tests: none known for this optimisation slice initially. After rebasing before reintegration, caco-tui tests no longer compiled because `TestJob` fixture literals in `crates/caco-tui/src/state/tests.rs` were missing newly-added `retry_after_secs` and `retry_command` fields; this was filed and fixed as `bd-82139f`.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `32b897cc7`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.5MB, app-side work FPS ≈374.0, terminal-inclusive work FPS ≈168.0, avg work ≈2.67ms, avg terminal-inclusive ≈5.95ms, avg upload pass ≈0.80ms.
- Context: after Harry’s correction, text-mode results were no longer used for graphics claims. A previous tiny `z_clause` allocation experiment passed byte-equivalence tests but was noisy/worse under actual graphics, so it was reverted and left as draft evidence.

## After state

- Failing tests: none observed in validation below. The `TestJob` fixture compile drift is fixed by adding `None` values for the new retry fields in both affected literals.
- Relevant metrics: actual Xvfb/kitty reruns after streaming base64 chunks stayed graphics-gated (`graphics_capability=Kitty`, uploads/deletes observed, terminal-sync enabled). First run: app-side work FPS ≈419.2, terminal-inclusive FPS ≈178.5, avg work ≈2.39ms, avg terminal-inclusive ≈5.60ms, avg upload pass ≈0.69ms. Rerun: app-side work FPS ≈452.0, terminal-inclusive FPS ≈184.4, avg work ≈2.21ms, avg terminal-inclusive ≈5.42ms, avg upload pass ≈0.67ms. Upload/deletes/wire stayed effectively the same.
- Context: kitty placement uploads no longer allocate a full base64 string for each image. Instead, raw payload is encoded in 3072-byte chunks that produce the same 4096-character base64 chunks as the previous full-string implementation.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/kitty.rs`, `crates/caco-tui/src/state/tests.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: no tests added or removed; existing byte-equivalence and kitty command tests cover output preservation, and existing state tests compile again after fixture field updates.
- Behavioural delta: no intended terminal protocol output change. Placement command bytes remain equivalent to the legacy full-string path; only the internal base64 encoding allocation strategy changes.
- Validation: `cargo test -p caco-tui placement_command_optimization_matches_legacy_byte_for_byte`; `cargo test -p caco-tui placement_command_optimization_perf_smoke`; `cargo test -p caco-tui placement_command_empty_data_is_reference`; `cargo test -p caco-tui kitty::tests::`; `cargo test -p caco-tui`; `git diff --check`; before/after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings; the separate `bd-82139f` compile drift is fixed in this diff.

## Operator-takeaway

This is the first post-correction optimiser slice backed by actual Kitty graphics and terminal-sync evidence. Streaming base64 chunks reduced upload-pass cost and improved terminal-inclusive work FPS while preserving byte-for-byte kitty command output.
