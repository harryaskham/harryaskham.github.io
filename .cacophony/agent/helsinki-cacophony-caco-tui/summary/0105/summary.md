# bd-2aa972: warn when graphics benchmark terminal sync dominates

## What changed

- `build_real_benchmark_result()` now adds an operator-facing `benchmark_warnings` entry when:
  - graphics are effectively enabled,
  - graphics work is observed,
  - terminal sync is enabled, and
  - terminal-inclusive work FPS is materially below app-side `work_fps` (<90%).
- Added regression coverage that synthetic Kitty graphics work with high terminal-sync cost emits the new warning.

## Why

The benchmark already prints terminal-inclusive metrics, but app-side `Work FPS` can still look healthy while Kitty/Ghostty spends most of the time processing graphics commands. The JSON warning makes direct `caco tui benchmark --json` output and wrapper artifacts harder to misread when graphics/text parity is only apparent because the terminal-side cost is separated from app-side work.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app/benchmark_support.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_2aa972"` — `tj-7a9f0cec`, passed
