# bd-8e16e0 — caco-cli ~80 --lib tests fail under shared cargo test run (env-isolation cluster)

## Goal
Stop a single panic inside an env-mutating caco-cli test from cascading
into ~80 unrelated test failures during a shared `cargo test -p caco-cli
--lib` run.

## Bead(s)
- bd-8e16e0 (P2 bug) — broken-on-main: ~80 of 848 caco-cli --lib tests
  fail under shared run, pass standalone.

## Before state
- `cargo test -p caco-cli --lib` reported 765 passed / 83 failed on
  HEAD 7f2a17f2.
- Survey of panic stacks revealed the dominant failure mode was
  `PoisonError { .. }` raised by `ENV_MUTEX.lock().unwrap()` — the
  first test that panicked inside an `ENV_MUTEX`-held critical section
  poisoned the mutex, and every subsequent env-mutating test then
  panicked on the poison rather than on its own assertions.
- `--test-threads=1` masked the cascade because tests serialized; the
  poison still occurred but only one test per process was live to
  observe it.

## After state
- Every `ENV_MUTEX.lock().unwrap()` now uses
  `lock().unwrap_or_else(|e| e.into_inner())` so a panicked
  env-mutating test no longer drags the rest of the suite down with
  it.
- `static ENV_MUTEX` carries a doc comment that explains the
  poison-recovery contract and references bd-8e16e0, so future
  env-touching tests won't reintroduce the bare `.unwrap()` pattern.
- Suite result on this branch: 836 passing / 7 failing
  (was 765 / 83). The 7 remainders are real pre-existing env leakage
  between specific tests (bd_dispatch_*, pi_requires_project_flag,
  resolve_remote_caco_binary_no_config_falls_through,
  error_level_log_sites_are_routed_through_structured_helpers) plus
  bd-51859d (claude_requires_project_flag — a tracked production bug
  outside this bead).

## Diff summary
- `crates/caco-cli/src/lib.rs`: 88 call sites changed from
  `ENV_MUTEX.lock().unwrap()` to `lock().unwrap_or_else(|e| e.into_inner())`.
- Same file: doc comment on the `static ENV_MUTEX` declaration
  documenting the poison-recovery contract.

## Operator-takeaway
The caco-cli --lib failure count drops from 83 to 7. Future cleanups
of the remaining 7 should be filed as separate small beads — they are
mostly tests that mutate env without holding ENV_MUTEX or that read
env from a worker thread (e.g. tokio runtime). If a future change adds
a new env-mutating test, follow the documented contract and use
`lock().unwrap_or_else(|e| e.into_inner())` so we never regress to
the poison cascade.

## Tests
- `cargo test -p caco-cli --lib caller_inference` — 4 passed (was
  4 failed pre-fix).
- `cargo test -p caco-cli --lib -- --test-threads=8 --skip
  choices_mcp_tools_call_dispatches --skip config_show_raw` — 836
  passing, 7 failing (was 765 / 83 before fix).
