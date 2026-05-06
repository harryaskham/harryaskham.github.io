# Session summary — clear stale queued-dispatch skip noise from daemon-crash.log

## Goal

Stop deterministic queued-dispatch pickup skips for already-in-progress beads from tight-looping into `daemon-crash.log`, while still preserving operator-visible diagnostics about the cleanup.

## Bead(s)

- `bd-11fdf4` — queued dispatch pickup skips for in-progress beads recur in daemon-crash.log
- related validation blocker observed during this session: `bd-9a46a4` — `[broken-on-main] caco-daemon tests fail with missing SttDaemons references in beads.rs`

## Before state

- Failing tests: targeted daemon test compilation remained blocked by unrelated broken-on-main `SttDaemons` test import failures in `crates/caco-daemon/src/beads.rs`.
- Relevant metrics: log-monitor reported the queued-dispatch non-open skip recurrence crossed 1000 occurrences in one sweep and `daemon-crash.log` reached 546639 bytes on ms-mac.
- Context: queued dispatch pickup attempted to re-claim beads that were already `in_progress`, emitted `claim_bead: invalid operation: bead ... is not open (status: in_progress)` to stderr on every recurrence, and left the stale queued dispatch intent in place so the same deterministic skip kept repeating.

## After state

- Failing tests: `cargo build -p caco-daemon` passed via queued build; targeted daemon test validation is still blocked by unrelated `bd-9a46a4` test-compilation failures.
- Relevant metrics: deterministic non-open queued-dispatch skips now clear the stale queued intent immediately and contribute to a bounded per-project warning emitted through `log_without_stderr_mirror` instead of per-occurrence stderr spam.
- Context: the daemon still preserves operator visibility through a warning in `daemon.log` / feed with a count and sample bead ids, but the repetitive deterministic skip class no longer needs to pollute `daemon-crash.log` like crash evidence.

## Diff summary

- Commits: `18ad3fb9b`, `a84a5f025`, `0f176e149`
- Files touched: `crates/caco-daemon/src/lib.rs`
- Tests: +0 / -0 / flipped 0
- Behavioural delta: queued-dispatch pickup now recognizes the deterministic `claim_bead ... not open (status: in_progress)` skip class, clears the stale dispatch metadata, and emits one bounded non-crash warning instead of repeating stderr for every sweep.

## Embedded artefacts

- `summary.md` — recorded summary for the reintegration.

## Operator-takeaway

This slice converts a noisy recurring failure mode into self-healing behavior: once a queued dispatch intent is stale because the bead is already in progress, the daemon clears it and records a bounded warning instead of letting the same skip masquerade as ongoing crash evidence.
