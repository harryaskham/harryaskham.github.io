# Session summary — queued job running progress logs

## Goal

Make queued test/build jobs less opaque during long Rust compile phases for `bd-13513c`, so `caco test logs` / `caco build logs` can show that a job has actually started even before the child process exits and durable stdout/stderr is finalized.

## Bead(s)

- `bd-13513c` — Show compile progress or last activity for queued test jobs

## Before state

- Failing tests: none known for this bead.
- Relevant metrics: the reported friction was a queued validation job compiling for several minutes while `show` only said running and `logs` returned no stdout/stderr yet.
- Context: test/build queue managers only populated stdout/stderr artifact paths after terminal completion, and `logs()` returned `None` for both queued and running jobs.

## After state

- Failing tests: none from targeted validation.
- Relevant metrics: targeted queued validation passed after one transient daemon-reachability enqueue failure and retry.
- Context: when a queued test/build job starts executing, the daemon now creates `stdout.log`/`stderr.log`, writes a running progress marker to stderr with started time, command, and cwd, stores the artifact paths on the running job, persists queue state, and allows `logs()` to read those artifacts while the job remains running.

## Diff summary

- Commits: `47629ac3cd` (implementation commit after first-party rebase; final landed squash SHA to be assigned by reintegration receipt).
- Files touched: `crates/caco-daemon/src/test_queue.rs`, `crates/caco-daemon/src/build_queue.rs`.
- Tests: added running-job progress marker tests for both queued test and build managers.
- Behavioural delta: `caco test logs` and `caco build logs` can now surface an explicit “queued test/build job … running since …” marker during long-running commands before final process output is captured.

## Operator-takeaway

Long queued compile phases should no longer look indistinguishable from a hung/no-output job: running jobs now have durable artifact paths and an immediate progress marker that the existing logs surfaces can display.
