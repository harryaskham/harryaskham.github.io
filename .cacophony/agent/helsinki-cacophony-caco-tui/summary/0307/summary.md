# Session summary — headline graphics activity rate

## Goal

Expose a headline graphics activity rate in real-TUI benchmark JSON so future Kitty/Ghostty optimizer runs can quickly see how much of the measured window actually exercised graphics work without manual division or scene-summary traversal.

## Bead(s)

- `bd-51f3ca` — Expose headline graphics frame rate in TUI benchmark JSON

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: top-level benchmark JSON exposed raw `frames_with_graphics`, but not `graphics_frame_rate`. Scene summaries already exposed both `frames_with_graphics` and `graphics_frame_rate`.
- Context: after recent headline byte/delete/tail metrics, run-wide graphics activity still required manual division by `total_frames` or walking scene summaries.

## After state

- Failing tests: none observed.
- Relevant metrics: actual Kitty verification at `/tmp/caco-fps-bd-51f3ca-after.json` reported `graphics_capability=Kitty`, `graphics_work_observed=true`, 802 frames, `frames_with_graphics=591`, new `graphics_frame_rate≈0.7369`, work FPS ≈393.6, and terminal-inclusive FPS ≈172.8. The expected `source_dirty` caveat was present because the benchmark binary was rebuilt from the dirty pre-commit tree.
- Context: a bounded visual evidence pass also recorded `media/kitty-real-tui-benchmark-bd-51f3ca.mp4` and paired JSON `/tmp/caco-fps-bd-51f3ca-video-pass.json`, confirming `graphics_capability=Kitty` and `graphics_work_observed=true` under capture overhead.

## Diff summary

- Commits: final branch/reintegration commit to be assigned by `caco agent reintegrate`.
- Files touched: `crates/caco-tui/src/app/benchmark_support.rs`, `.cacophony/profiles/tui-animation-optimiser.md`.
- Tests: extended the scene cache/upload/terminal timing benchmark-support test to assert top-level `frames_with_graphics` and `graphics_frame_rate`; no tests removed.
- Behavioural delta: real-TUI benchmark JSON now includes top-level `graphics_frame_rate`, computed as `frames_with_graphics / total_frames`. Rendering, upload, and terminal synchronization behaviour are unchanged.
- Validation: `./scripts/rustfmt-changed.sh`; `git diff --check`; queued `cargo check -p caco-tui` (`tj-00a564ce`); queued focused graphics-frame-rate test (`tj-a9e8e600`); queued `cargo test -p caco-tui app::benchmark_support::tests` (`tj-9f5d8f07`); actual Kitty benchmark JSON check (`/tmp/caco-fps-bd-51f3ca-after.json`); bounded Kitty visual capture and paired JSON (`media/kitty-real-tui-benchmark-bd-51f3ca.mp4`, `/tmp/caco-fps-bd-51f3ca-video-pass.json`); queued `cargo clippy -p caco-tui --lib -- -D warnings` (`tj-eb9940d8`); queued `cargo test -p caco-tui` (`tj-2e65d749`).

## Embedded artefacts

- `media/kitty-real-tui-benchmark-bd-51f3ca.mp4` — compact Xvfb Kitty visual capture of the real-TUI benchmark surface, H.264 960x540 at 5 fps, 6.2s, 203210 bytes; paired JSON was written to `/tmp/caco-fps-bd-51f3ca-video-pass.json` during this run.

## Operator-takeaway

Headline real-TUI graphics benchmark JSON now includes the run-wide graphics activity rate, and the summary includes the bounded Kitty visual artefact proving the intended terminal surface was exercised.
