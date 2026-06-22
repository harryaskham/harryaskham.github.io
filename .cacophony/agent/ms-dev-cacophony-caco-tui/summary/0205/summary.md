# Session summary — keystroke-benchmark debug-build warning (bd-2ed8f3); input-lag "regression" proven a debug artifact (bd-7c03ee)

## Goal

Investigate and (per caco-ctrl routing) drive down TUI per-keystroke input latency
(bd-7c03ee, the concrete fix for the bd-c0ebb6 input-lag lineage). Profiling first
revealed the reported "~17.7ms regression" is a debug-build measurement artifact —
release/production is already fast — so the high-blast-radius render change was
correctly NOT made. The landed deliverable is a small, low-risk tooling guardrail
that makes the keystroke-benchmark warn when run in a debug build, so the next
agent/operator does not chase the same phantom.

## Bead(s)

- `bd-2ed8f3` — caco tui keystroke-benchmark: warn when run in a debug build
  (debug latency ~10x inflated, misreads as a regression). [landed this session]
- `bd-7c03ee` — Drive TUI per-keystroke input latency down. [investigated + closed
  this session as "no production regression — debug-mode measurement artifact";
  admin-override, no fix commit because no fix was warranted]
- lineage: parent `bd-687c93` (benchmark + baseline, closed by sgu24);
  `bd-c0ebb6` (P0 input-lag during pane updates — the real remaining target, is
  GRAPHICS/kitty-mode, not textmode; needs the operator's kitty terminal).

## Before state

- Failing tests: none introduced.
- bd-7c03ee premise: a debug-mode baseline (~17.7ms avg / p99 ~32 / 156-of-200
  keystrokes >16ms) was filed as a real textmode render regression, pointing at a
  high-blast-radius `App::render` memoization on the live production render path.
- `caco tui keystroke-benchmark` surfaced no indication of which build profile it
  ran under, so a debug measurement looked like a release regression.

## After state

- Failing tests: none.
- Measured keystroke-benchmark on current main, identical config (200x50, 200
  keystrokes, background_paint), debug vs release:
  - RELEASE (ships to operators): avg 1.47ms, p50 1.24, p95 2.09, p99 2.46,
    max 2.58, slow_keystrokes_16ms = 0.
  - DEBUG (same config): avg 11.9ms, p50 10.7, p95 16.4, p99 17.0, max 19.1,
    slow_keystrokes_16ms = 18.
  - => the ~17.7ms baseline is a DEBUG artifact (~8-12x slower than release);
    release textmode keystroke latency is already excellent (sub-3ms p99, 0 slow).
    No production regression; no render change made.
- `KeystrokeLatencyResult` now carries `debug_build: bool` (via
  `cfg!(debug_assertions)`); a debug run emits a stderr WARNING that debug latency
  is ~8-12x inflated vs release and to re-run with `--release`. JSON on stdout is
  unchanged in correctness (the warning is stderr-only).

## Diff summary

- Code/content commits: see reintegration receipt for the final landed SHA(s).
- Summary artefact commit: intentionally omitted (must not self-reference its SHA).
- Files touched: `crates/caco-tui/src/app/benchmark_support.rs` (+28 lines):
  - `KeystrokeLatencyResult` gains `pub debug_build: bool`.
  - `run_keystroke_latency_benchmark` sets `debug_build = cfg!(debug_assertions)`
    and eprintln!s a debug-build warning to stderr when true.
  - existing test `keystroke_latency_benchmark_runs_headless_and_reports_latencies_bd_687c93`
    gains `assert_eq!(result.debug_build, cfg!(debug_assertions))`.
- Tests: +1 assertion (no new test fn); 0 removed; 0 flipped.
- Behavioural delta: benchmark now self-labels its build profile + warns on debug;
  no change to TUI rendering, input handling, or any production path.
- Validation (caco-tui profile is ungated → manual gated-equivalent, proportionate
  to a low-blast-radius tooling change): focused caco-tui test passed via the
  daemon queue with a real compile + the test name in the job log; multi-crate
  compile exercised (clippy built caco-tui + caco-daemon); the change is clippy-clean
  on the touched file (the `-D warnings` failure is pre-existing caco-daemon
  webhooks.rs dependency lint debt, unrelated). Baseline/debug measurements were
  themselves run through the queue (release build 38m under burndown load; debug
  re-measure via the light caco-tui test path).

## Operator-takeaway

The TUI is not slow to type into in production: in a release build, textmode
per-keystroke latency is ~1.5ms (p99 2.5ms, zero keystrokes over the 16ms/60fps
budget). The alarming "~17.7ms" number was a debug-build measurement (debug runs
the render path ~10x slower than release). Profiling-before-optimizing avoided a
risky change to the live render path for a non-problem. The benchmark now warns
when run in debug so this ghost is not chased again. The genuine remaining input-lag
signal (bd-c0ebb6, "sluggish during pane updates") is in GRAPHICS/kitty mode, which
the textmode benchmark does not capture and which needs the operator's kitty
terminal to reproduce and verify.
