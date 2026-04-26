# Session summary — daemon hardening against stranded helpers

## Goal

Reduce the chance that Cacophony-owned queued work or lifecycle test helpers can outlive their parent and later interfere with the production daemon, and make existing stranded fake-daemon helpers visible in `caco doctor`.

## Bead(s)

- `bd-c5884d` — Harden caco daemon against worker/test process kill or port-takeover
- `bd-b225f9` — [broken-on-main] doctor_schema_detects_missing_and_extra_columns failing

## Before state

- Failing tests: `doctor_schema_detects_missing_and_extra_columns` expected `caco doctor schema --json` to report `ok=true` even when drift exists, contradicting the current semantic-ok contract.
- Relevant metrics: queued test/build jobs already launched with stdin null and isolated process groups, but on Linux they did not request parent-death cleanup; `caco doctor` did not surface `caco-lifecycle-iso-*/fake-daemon.sh` leftovers.
- Context: operator evidence showed orphaned fake-daemon shells and `nc` children could persist after hard parent death and squat daemon ports.

## After state

- Failing tests: none in focused validation.
- Relevant metrics: queued test/build children now also request Linux `PR_SET_PDEATHSIG(SIGTERM)` and exit if already orphaned at pre-exec time; `caco doctor` adds a lifecycle warning for stranded lifecycle fake-daemon processes with sample PIDs and cleanup guidance.
- Context: the stale doctor schema test now expects `ok=false` when drift is present, matching the documented bd-44af89 behavior.

## Diff summary

- Commits: `5573028bf`
- Files touched: `SPEC.md`, `crates/caco-cli/src/lib.rs`, `crates/caco-daemon/src/test_queue.rs`, `crates/caco-daemon/src/build_queue.rs`
- Tests: `cargo fmt --all -- --check`; `cargo test -p caco-cli parses_only_lifecycle_fake_daemon_processes_bd_c5884d --lib`; `cargo test -p caco-cli doctor_schema_detects_missing_and_extra_columns --lib`; `cargo check -p caco-daemon`; `git diff --check`
- Behavioural delta: doctor reports lifecycle fake-daemon leftovers, and queued test/build process trees get an additional Linux kernel-level parent-death guard.

## Operator-takeaway

This is a defensive hardening slice: it does not make workers unable to signal every daemon process, but it closes the concrete stale-helper leak path observed on helsinki and makes future recurrences visible through first-party doctor output.
