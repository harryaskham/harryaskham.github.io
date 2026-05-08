# Session summary — queued-test restart retry metadata

## Goal

Stabilize the operator/agent experience when daemon restarts interrupt queued test jobs. The implementation keeps the existing safety decision that a running child process cannot be trusted after daemon death, but makes the retry path explicit and machine-readable so workers do not have to invent backoff or reconstruct the original command manually.

## Bead(s)

- `bd-b70364` — Stabilize queued tests across daemon restart windows

## Before state

- Failing tests: none owned by this bead; unrelated broken-on-main docs/TUI failures were announced and owned by other agents.
- Relevant metrics: recovered running queued-test jobs already surfaced `retryable: true`, `failure_kind: "daemon_restart_recovered"`, artifact paths, and a prose recovery hint.
- Context: broader queued `caco-sidecar` validations had returned retryable `daemon_restart_recovered` outcomes during daemon restart windows, leaving agents with manual retry/backoff decisions.

## After state

- Failing tests: none from focused validation.
- Relevant metrics: queued validation `tj-fdc8e0cb` passed `RUST_BACKTRACE=1 cargo test -p caco-daemon bd_b70364 --lib -- --nocapture && cargo check -p caco-cli` with exit code 0 after one daemon-restart-recovered retry and one empty-log queue failure retry.
- Context: recovered queued-test restart errors now include advisory `retry_after_secs` and an exact first-party `retry_command` that re-submits the same project/cwd/command, and human `caco test run --wait` / `caco test show` output displays both fields.

## Diff summary

- Commits: `11d9a6aee1`, `d480bf969d`
- Files touched: `crates/caco-daemon/src/test_queue.rs`, `crates/caco-cli/src/lib.rs`, `SPEC.md`, `README.md`, `AGENTS.md`
- Tests: +2 daemon unit tests / -0 / flipped 0
- Behavioural delta: daemon restart recovery for previously-running queued tests remains terminal retryable infrastructure error, but now carries concrete retry scheduling metadata; CLI human output surfaces that metadata alongside existing retryability/failure-kind fields.

## Operator-takeaway

The queue still refuses to claim a trustworthy validation result after daemon death, which is the safe behavior, but agents and automation now get a direct retry command and a minimum stability delay instead of a vague manual hint.
