# Session summary — headline upload byte density

## Goal

Expose headline upload payload and wire-byte density in real-TUI benchmark JSON so future Kitty/Ghostty optimizer runs can compare upload byte pressure across runs without manual arithmetic or scene-summary traversal.

## Bead(s)

- `bd-8cbd3e` — Expose headline upload byte density in TUI benchmark JSON

## Before state

- Failing tests: none known for this slice. One queued `cargo check -p caco-tui` attempt (`tj-463ae4ff`) ended with retryable `daemon_restart_recovered`; retry passed and this was infrastructure, not a code failure.
- Relevant metrics: top-level benchmark JSON exposed `upload_wire_bytes` plus upload count density, but not raw payload bytes or payload/wire bytes per frame. Scene summaries already exposed `upload_bytes`, `upload_bytes_per_frame`, `upload_wire_bytes`, and `upload_wire_bytes_per_frame`.
- Context: after recent headline tail-metric slices, byte-pressure analysis still required dividing top-level wire bytes by frame count manually or walking scene summaries.

## After state

- Failing tests: none observed.
- Relevant metrics: actual Kitty verification at `/tmp/caco-fps-bd-8cbd3e-after.json` reported `graphics_capability=Kitty`, `graphics_work_observed=true`, 841 frames, work FPS ≈440.1, terminal-inclusive FPS ≈181.4, `upload_bytes=19853558`, `upload_bytes_per_frame≈23607.1`, `upload_wire_bytes=26546596`, and `upload_wire_bytes_per_frame≈31565.5`. The expected `source_dirty` caveat was present because the benchmark binary was rebuilt from the dirty pre-commit tree.
- Context: a bounded visual evidence pass also recorded `media/kitty-real-tui-benchmark-bd-8cbd3e.mp4` and paired JSON `/tmp/caco-fps-bd-8cbd3e-video-pass.json`, confirming `graphics_capability=Kitty` and `graphics_work_observed=true` under capture overhead.

## Diff summary

- Commits: final branch/reintegration commit to be assigned by `caco agent reintegrate`.
- Files touched: `crates/caco-tui/src/app/benchmark_support.rs`, `.cacophony/profiles/tui-animation-optimiser.md`.
- Tests: extended a focused benchmark-support test to assert headline `upload_bytes`, `upload_bytes_per_frame`, `upload_wire_bytes`, and `upload_wire_bytes_per_frame`; no tests removed.
- Behavioural delta: real-TUI benchmark JSON now includes top-level payload byte totals and payload/wire byte density per measured frame. Rendering, upload, and terminal synchronization behaviour are unchanged.
- Validation: `./scripts/rustfmt-changed.sh`; `git diff --check`; queued `cargo check -p caco-tui` (`tj-bd3dc55a`, after retryable infra `tj-463ae4ff`); queued focused upload byte-density test (`tj-ea3960fd`); queued `cargo test -p caco-tui app::benchmark_support::tests` (`tj-630ce33d`); actual Kitty benchmark JSON check (`/tmp/caco-fps-bd-8cbd3e-after.json`); bounded Kitty visual capture and paired JSON (`media/kitty-real-tui-benchmark-bd-8cbd3e.mp4`, `/tmp/caco-fps-bd-8cbd3e-video-pass.json`); queued `cargo clippy -p caco-tui --lib -- -D warnings` (`tj-cb0cd3df`); queued `cargo test -p caco-tui` (`tj-0ceac359`).

## Embedded artefacts

- `media/kitty-real-tui-benchmark-bd-8cbd3e.mp4` — compact Xvfb Kitty visual capture of the real-TUI benchmark surface, H.264 960x540 at 5 fps, 6.2s, 195509 bytes; paired JSON was written to `/tmp/caco-fps-bd-8cbd3e-video-pass.json` during this run.

## Operator-takeaway

Headline real-TUI graphics benchmark JSON now includes both payload and wire-byte pressure per frame, and the summary includes the bounded Kitty visual artefact required to prove the intended terminal surface was exercised.
