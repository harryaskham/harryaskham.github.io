# Session summary — bd-2f6ce3: android-build-gate broken-pipe panic unblock

## Goal

Fix the P1 bug where the `android-build-gate` before_reintegration hook panicked
with `failed printing to stderr: Broken pipe`, which was misclassified as a build
failure and **blocked all android-lane reintegrations**.

## Bead(s)

- `bd-2f6ce3` — [android] android-build-gate before_reintegration hook panics:
  'handler panicked: failed printing to stderr: Broken pipe' — blocks android
  lands (filed by aurora-cacophony-caco-android-releaser).

## Before state

- Failing: every android-lane reintegration composing the gate. The gate ran
  `caco build run --wait --command "gradle ... assembleRelease" >&2`, so caco
  build run's stderr was the gate's inherited pipe to the reintegration runner.
  When that reader closed during the multi-minute build, the CLI's next stderr
  write hit EPIPE and panicked (`handler panicked: failed printing to stderr:
  Broken pipe (os error 32)`), the gate printed `BUILD FAILED (queue)`, and the
  land was blocked — even though the gradle build itself was fine (a queue/IO
  transport panic, not a compile failure).

## After state

- `android-build-gate.sh` queue path now redirects caco build run's stdout+stderr
  to a regular temp file (`>"$gate_log" 2>&1`) instead of the gate's stderr — a
  regular file has no closeable pipe, so the EPIPE source is removed. Output is
  `cat`'d to stderr for visibility; pass/fail is decided by the exit code.
- Belt-and-suspenders: any residual broken-pipe/EPIPE handler panic in the log
  is classified as queue/transport infra (not a gradle failure) and falls back
  to the inline build for a real answer rather than blocking on a false fail.
  Genuine gradle failures are still detected and block as before.
- Validation: `bash -n` clean; `git diff --check` clean; the panic-vs-real-
  failure classification grep was functionally tested (EPIPE panic → detected/
  fallback; real `compileReleaseKotlin FAILED` → not detected/block; phrasing
  variant → detected).

## Diff summary

- Code commit: bd-2f6ce3 (this change); final landed squash SHA from the receipt.
- Files touched: `plugins/caco-agent/agents/android-build-gate.sh` (queue-path
  file redirect + EPIPE-panic classification + inline fallback flags).
- Tests: shell hook — no unit test; `bash -n` + functional classification check.
- Behavioural delta: a broken stderr pipe during the queued android build no
  longer panics into a false BUILD FAILED that blocks the land.

## Operator-takeaway

The android land gate was self-blocking on an IO transport panic, not a real
build break: caco build run streamed to a pipe that the reintegration runner
could close mid-build, and `eprintln!` panics on EPIPE. Capturing caco build run
to a file removes that pipe. The deeper root cause — `caco build run` itself
panicking on a broken stderr instead of tolerating EPIPE — remains a worthwhile
daemon/queue-owner follow-up (it would protect every queued-build caller), but
the gate-side fix unblocks android lands now.
