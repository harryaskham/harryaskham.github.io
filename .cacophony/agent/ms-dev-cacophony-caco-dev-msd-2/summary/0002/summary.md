# Route stale test tmux sessions away from ambiguous_ownership (bd-62b44e)

## Goal

Stop transient test-session leaks from flagging runtime repair as
degraded. Operator-visible symptom: `caco status` showed "ambiguous
ownership: 43" even when the cited sessions were `caco-testnode-*` /
`caco-test-runtime-*` — not actionable, but degrading health.

## Bead(s)

- `bd-62b44e` — Runtime repair health degrades on ambiguous tmux
  ownership events for test sessions.

## Before state

In `repair_stale_tmux_sessions` (`crates/caco-daemon/src/agent/lifecycle.rs`),
any session that didn't match Case 1/1b/2 (terminal-agent, id-suffix
match, ownership file) fell into Case 3 → `record_ambiguous_ownership`,
which **always** sets `RuntimeRepairSummary::degraded = true`. Test
sessions leaked from crashed test processes (`caco-testnode-{proj}-{name}`,
`caco-test-runtime-{pid}`) accumulate naturally on shared sockets,
poison the ambiguous-ownership budget, and trigger degraded status
indefinitely until someone manually reaps them.

## After state

New helper `agent::is_test_session_name(&str)` recognises the two
documented test-session prefixes. The reconciler routes those into
a new repair-event kind `test_session_skipped` (handled by
`RuntimeRepairSummary::record_test_session_skipped`) which:

1. Increments a separate `test_sessions_skipped: u32` counter.
2. Pushes a `RepairEvent { kind: "test_session_skipped", … }` for
   visibility (so an unexpected spike is still observable in
   `caco status` and the TUI status surface).
3. **Does NOT set `degraded`** — these are not managed runtime; they
   either get torn down by another in-flight test or harmlessly
   linger.
4. Adds the session to the ambiguous-dedup set so the skip event
   doesn't re-emit every reconcile cycle.

Real ambiguous-ownership cases (managed sockets, no ownership file,
no test-prefix match) still degrade exactly as before.

## Diff summary

- `crates/caco-daemon/src/agent/mod.rs`: new `pub fn
  is_test_session_name(session: &str) -> bool` after
  `extract_known_agent_id`. Single source of truth for test prefixes.
- `crates/caco-daemon/src/agent/types.rs`: new
  `test_sessions_skipped: u32` field on `RuntimeRepairSummary`,
  new `record_test_session_skipped` method (NON-degrading), updated
  `has_events()` to include the new counter.
- `crates/caco-daemon/src/agent/lifecycle.rs`:
  - In `repair_stale_tmux_sessions` Case 3 path, check
    `is_test_session_name(session)` before falling through to
    `ambiguous_ownership`. If matched, push a `test_session_skipped`
    repair action with the same dedup behaviour.
  - In the apply-actions loop, route the new kind to
    `record_test_session_skipped`.
- `crates/caco-daemon/src/agent/tests.rs`: 5 new tests
  (`is_test_session_name_recognises_testnode_prefix`,
  `is_test_session_name_recognises_test_runtime_prefix`,
  `is_test_session_name_rejects_real_agent_sessions`,
  `record_test_session_skipped_does_not_set_degraded`,
  `record_test_session_skipped_then_ambiguous_only_one_degrades`).

## Operator-takeaway

Health pass on a node with stale test sessions reports
`test_sessions_skipped: N` instead of `ambiguous_ownership: N` and
no longer flags repair as degraded for that reason alone. Real
ambiguous ownership still degrades; the budget is now isolated from
test-process churn. Acceptance criteria 1, 2, and 4 from the bead
are met directly; criterion 3 (existing acceptance: not held
degraded solely due to stale test sessions) is the headline outcome.
