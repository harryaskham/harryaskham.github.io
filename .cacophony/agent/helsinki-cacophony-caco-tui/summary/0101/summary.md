# bd-797d0c: fix terminal-inclusive benchmark double counting

## What changed

- Real TUI benchmark loop now captures `app_frame_time = frame_start.elapsed()` before running the optional terminal sync barrier.
- `frame_times` and per-scene frame samples now record app-side work only.
- `terminal_sync_times` remains separate and terminal-inclusive metrics add sync time exactly once.
- Added regression coverage ensuring the app frame sample is captured before terminal sync and no `total_frame_time` sample is recorded after sync.

## Why

After terminal-inclusive metrics were added, the benchmark loop still sampled `frame_times` after the terminal sync barrier, then `build_real_benchmark_result()` added `terminal_sync_times` again for terminal-inclusive FPS. That double-counted terminal-side cost and could make the new metrics overly pessimistic. Separating app work from terminal sync makes `Work FPS` and `Terminal+Work FPS` truthful.

## Validation

- Initial targeted run caught a remaining scene sample that still referenced the old `total_frame_time` variable.
- Fixed scene samples to use `app_frame_time` too.
- `rustfmt --edition 2021 --check crates/caco-tui/src/app/benchmark_support.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_797d0c"` — `tj-55d5a0c7`, passed
