# Session summary — fast-test-gate timeout guard

## Goal

Prevent fast-test-gate from hanging indefinitely when cargo test-small
or cargo clippy stalls under memory pressure from concurrent agent
compilation.

## Bead(s)

- `bd-b919ec` — cargo test-small regularly takes >3600s

## Before state

- `fast-test-gate.sh` ran test, compile-check, and clippy steps with
  no timeout. Under concurrent compilation (5+ agents on the same
  host), memory pressure caused swap storms where a single
  `cargo test-small` stalled for >3600s with no feedback.
- The gate used bare `eval` for test/check commands.

## After state

- Each gate step now has a configurable timeout (default 600s):
  `CACO_REINTEGRATION_TEST_TIMEOUT`,
  `CACO_REINTEGRATION_CHECK_TIMEOUT`,
  `CACO_REINTEGRATION_CLIPPY_TIMEOUT`.
- Uses coreutils `timeout` with `gtimeout` fallback (macOS).
  Falls back to unbounded if neither is available.
- Exit code 124 produces a clear `TIMED OUT` message referencing
  bd-b919ec and memory pressure.
- `eval` replaced with `bash -c` for timeout compatibility.
- Profile docs updated with the three new env vars.
- `bash -n` syntax check: clean.
- `cargo test-small`: 252/252 passed in 3s.

## Diff summary

- Files touched:
  - `plugins/caco-agent/agents/fast-test-gate.sh` (+44 / -10)
  - `.cacophony/profiles/cacophony-fast-tests.md` (+3 / -1)
- Behavioural delta: gate steps now fail-fast after 600s instead of
  hanging indefinitely. The timeout is per-step, not cumulative.

## Operator-takeaway

The >3600s hangs were caused by memory pressure when 5+ agents
compiled concurrently, not by a test bug. The fix bounds the blast
radius: a stalled gate reports TIMED OUT after 10 minutes instead
of blocking the agent forever. If timeouts fire frequently, the
operator should reduce agent concurrency or add RAM — the timeout
is a symptom cap, not a root-cause fix.
