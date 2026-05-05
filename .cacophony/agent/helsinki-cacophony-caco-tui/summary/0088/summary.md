# bd-e8f1e0: keep text benchmarks from paying graphics animation ticks

## What changed

- The real TUI benchmark loop now injects a synthetic `TuiEvent::AnimationFrame` only when the surface manager is effectively graphics-capable and animations are enabled.
- Text/ASCII benchmark baselines (`tui.graphics.enabled: false`) skip graphics animation event handling entirely.
- Added a regression assertion that the benchmark animation event is guarded by effective graphics capability.

## Why

The real benchmark loop uses synthetic animation frames to exercise graphics animations without a live event thread. That is appropriate for graphics runs, but it was also executed for text baselines. A graphics-disabled live TUI would not schedule the graphics animation thread, so text-vs-graphics comparisons were making the text baseline pay border/background phase-check overhead that does not belong to ASCII/text mode. Gating the synthetic event keeps benchmark baselines cleaner and parity evidence less misleading.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app/benchmark_support.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_e8f1e0"` — `tj-b42685de`, passed
