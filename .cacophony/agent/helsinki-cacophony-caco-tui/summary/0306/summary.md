# Session summary — headline delete density

## Goal

Expose headline kitty delete-command density and delete failures in real-TUI benchmark JSON so future Kitty/Ghostty optimizer runs can compare cleanup-command churn across runs without manual arithmetic or scene-summary traversal.

## Bead(s)

- `bd-663502` — Expose headline delete density in TUI benchmark JSON

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: top-level benchmark JSON exposed `deletes_sent`, but not `deletes_per_frame`, `delete_failures`, or `delete_failures_per_frame`. Scene summaries already exposed those fields.
- Context: after recent upload-byte and terminal/upload tail metric additions, headline benchmark JSON still required scene traversal to evaluate kitty delete-command churn and failure density.

## After state

- Failing tests: none observed.
- Relevant metrics: actual Kitty verification at `/tmp/caco-fps-bd-663502-after.json` reported `graphics_capability=Kitty`, `graphics_work_observed=true`, 708 frames, work FPS ≈302.5, terminal-inclusive FPS ≈153.3, `deletes_sent=237`, new `deletes_per_frame≈0.3347`, new `delete_failures=0`, and new `delete_failures_per_frame=0.0`. The expected `source_dirty` caveat was present because the benchmark binary was rebuilt from the dirty pre-commit tree.
- Context: a bounded visual evidence pass also recorded `media/kitty-real-tui-benchmark-bd-663502.mp4` and paired JSON `/tmp/caco-fps-bd-663502-video-pass.json`, confirming `graphics_capability=Kitty` and `graphics_work_observed=true` under capture overhead.

## Diff summary

- Commits: final branch/reintegration commit to be assigned by `caco agent reintegrate`.
- Files touched: `crates/caco-tui/src/app/benchmark_support.rs`, `.cacophony/profiles/tui-animation-optimiser.md`.
- Tests: extended a focused benchmark-support test to assert headline delete count/density/failures alongside existing upload byte-density assertions; no tests removed.
- Behavioural delta: real-TUI benchmark JSON now includes top-level `deletes_per_frame`, `delete_failures`, and `delete_failures_per_frame`. Rendering, upload, and terminal synchronization behaviour are unchanged.
- Validation: `./scripts/rustfmt-changed.sh`; `git diff --check`; queued `cargo check -p caco-tui` (`tj-9a805248`); queued focused delete-density test (`tj-83eec3e8`); queued `cargo test -p caco-tui app::benchmark_support::tests` (`tj-e72f81a5`); actual Kitty benchmark JSON check (`/tmp/caco-fps-bd-663502-after.json`); bounded Kitty visual capture and paired JSON (`media/kitty-real-tui-benchmark-bd-663502.mp4`, `/tmp/caco-fps-bd-663502-video-pass.json`); queued `cargo clippy -p caco-tui --lib -- -D warnings` (`tj-4491db52`); queued `cargo test -p caco-tui` (`tj-195b068f`).

## Embedded artefacts

- `media/kitty-real-tui-benchmark-bd-663502.mp4` — compact Xvfb Kitty visual capture of the real-TUI benchmark surface, H.264 960x540 at 5 fps, 5.4s, 210035 bytes; paired JSON was written to `/tmp/caco-fps-bd-663502-video-pass.json` during this run.

## Operator-takeaway

Headline real-TUI graphics benchmark JSON now includes delete-command density and failure rates, and the summary includes the bounded Kitty visual artefact proving the intended terminal surface was exercised.
