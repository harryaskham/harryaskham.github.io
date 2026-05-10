# Session summary — headline render-only tail metrics

## Goal

Expose headline render-only tail timing in real-TUI benchmark JSON so future Kitty/Ghostty optimizer runs can compare app/render-side spikes against upload-pass and terminal-sync spikes without raw frame trace analysis.

## Bead(s)

- `bd-66de26` — Expose headline render-only tail metrics in TUI benchmark JSON

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: top-level benchmark JSON exposed `avg_render_only_ms` and `p95_render_only_ms`, while upload-pass and terminal-sync phases already exposed p95/p99/max/slow-frame headline fields.
- Context: an isolated render-side spike could be hidden behind p95 and require custom analysis or raw timing traces to compare with upload/terminal high-tail costs.

## After state

- Failing tests: none observed.
- Relevant metrics: actual Kitty verification at `/tmp/caco-fps-bd-66de26-after.json` reported `graphics_capability=Kitty`, `graphics_work_observed=true`, 619 frames, `avg_render_only_ms≈2.7003`, `p95_render_only_ms≈2.6257`, new `p99_render_only_ms≈3.1902`, new `max_render_only_ms≈336.8542`, work FPS ≈260.9, and terminal-inclusive FPS ≈141.2. The expected `source_dirty` caveat was present because the benchmark binary was rebuilt from the dirty pre-commit tree.
- Context: a bounded visual evidence pass also recorded `media/kitty-real-tui-benchmark-bd-66de26.mp4` and paired JSON `/tmp/caco-fps-bd-66de26-video-pass.json`, confirming `graphics_capability=Kitty`, `graphics_work_observed=true`, and the new render-tail fields under capture overhead.

## Diff summary

- Commits: final branch/reintegration commit to be assigned by `caco agent reintegrate`.
- Files touched: `crates/caco-tui/src/app/benchmark_support.rs`, `.cacophony/profiles/tui-animation-optimiser.md`.
- Tests: extended benchmark-support assertions for top-level render-only average, p95, new p99, and new max fields; no tests removed.
- Behavioural delta: real-TUI benchmark JSON now includes top-level `p99_render_only_ms` and `max_render_only_ms` derived from existing render-time samples. Rendering, upload, terminal synchronization, and benchmark pacing behaviour are unchanged.
- Validation: `./scripts/rustfmt-changed.sh`; `git diff --check`; queued `cargo check -p caco-tui` (`tj-31eca899`); queued focused render-tail test (`tj-ca01c442`); queued `cargo test -p caco-tui app::benchmark_support::tests` (`tj-6ca3e147`); actual Kitty benchmark JSON check (`/tmp/caco-fps-bd-66de26-after.json`); bounded Kitty visual capture and paired JSON (`media/kitty-real-tui-benchmark-bd-66de26.mp4`, `/tmp/caco-fps-bd-66de26-video-pass.json`); queued `cargo clippy -p caco-tui --lib -- -D warnings` (`tj-269160b5`); queued `cargo test -p caco-tui` (`tj-b891b201`).

## Embedded artefacts

- `media/kitty-real-tui-benchmark-bd-66de26.mp4` — compact Xvfb Kitty visual capture of the real-TUI benchmark surface, H.264 960x540 at 5 fps, 5.0s, 186210 bytes; paired JSON was written to `/tmp/caco-fps-bd-66de26-video-pass.json` during this run.

## Operator-takeaway

Headline real-TUI benchmark JSON now shows render-only p99 and max timing, making app/render spikes directly comparable with upload-pass and terminal-sync tails in future graphics investigations.
