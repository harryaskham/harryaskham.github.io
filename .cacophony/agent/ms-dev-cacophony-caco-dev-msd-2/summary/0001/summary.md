# Session summary — bd-6bdb17 checkout_bootstrap ERR trap reports real exit code

## Goal

Fix the long-standing "(exit 0) reported as failure" parenthetical
of bd-6bdb17 — every real bootstrap failure on persistent agents
(observed: sgu24 pi-install retry loop) was logged with `(exit 0)`,
making the diagnostic output useless for triage.

## Bead(s)

- `bd-6bdb17` — checkout_bootstrap pi-preinstall step doesn't re-run
  for agents with pre-existing dirs — legacy agents stuck in retry
  loop (also: 'exit 0' reported as failure) (P1 bug)

## Before state

- Bootstrap ERR trap body:
  `trap 'echo >&2 "..."; echo "...(exit $?)" >> log' ERR`
  read `$?` AFTER the first echo had overwritten it with 0.
- Sgu24 `node-ctrl-sgu` retry loop and the helsinki persistent flood
  both surface the same misleading `(exit 0)` line in operator-visible
  logs.
- Failing tests: none.

## After state

- Trap now captures `_caco_bootstrap_ec=$?` as its first statement and
  references that captured value in both stderr and the persistent
  bootstrap log.
- Existing `init_script_writes_bootstrap_log_on_failure` still passes
  unchanged.
- New `init_script_bootstrap_failure_records_real_exit_code_not_zero`
  pins the contract: snippet `(exit 42)` produces `(exit 42)` in
  stderr and `bootstrap.log`, and explicitly asserts neither stream
  contains `(exit 0)` for a real failure.
- Failing tests: none. All 29 bootstrap tests in `caco-daemon` pass.

## Diff summary

- Commit: `4c68b238`
- Files touched:
  - `crates/caco-daemon/src/agent/spawn.rs` — single-line trap rewrite
  - `crates/caco-daemon/src/agent/tests.rs` — +1 regression test
- Tests: +1 unit test (`init_script_bootstrap_failure_records_real_exit_code_not_zero`)
- Behavioural delta: the `checkout_bootstrap failed at: ...` line
  emitted to stderr and `agent_dir/logs/bootstrap.log` now carries the
  failing snippet's true exit code instead of always `(exit 0)`.

## Operator-takeaway

This unblocks triage of the recurring sgu24 / helsinki persistent-agent
retry storms — operators reading `bootstrap.log` will now see the real
exit code from the failing `pi install` (or whichever step) and can
distinguish "exit 1 from a network blip" from "exit 127 missing binary"
from "exit 130 user interrupt", instead of every entry blending into
`(exit 0)`.

The bead's broader retry-storm acceptance is already largely covered by
work that landed earlier in the day:
- declarative pruning runs in periodic-reconcile Phase 4 (bd-918a49 /
  bd-e9f319),
- exponential backoff is in `PersistentSentinel::mark_failed`,
- per-fingerprint event collapse landed in bd-07bd29 yesterday via
  `error_rate_limit::ErrorRateLimiter`.

The remaining unaddressed item — "agents in config but whose target
node is no longer reachable: mark stopped_node_unavailable, no retries
until node returns" — is a larger declarative-state design change and
should be filed as a separate bead rather than bundled.
