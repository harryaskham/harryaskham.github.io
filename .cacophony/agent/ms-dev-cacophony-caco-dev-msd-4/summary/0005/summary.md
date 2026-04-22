# Session summary — bd-102eb0 burndown spawn-fail trilogy

## Goal

Stop burndown's per-project active cap from producing a phantom
`mode_spawn_failed` agent in the feed, enforce the rule's
`max_concurrency` guard synchronously, and replace the 30s tight
retry-loop on a wedged spawn with exponential backoff plus an
operator-visible notification.

## Bead(s)

- `bd-102eb0` — Burndown `mode_spawn_failed`: synthetic FeedEvent
  ID `NODE:PROJECT:mode_spawn_failed` surfaces as a phantom agent;
  `in_flight` tracking is dead code; failure-loop on cap.

## Before state

- Spawn-failure feed event used the literal string
  `NODE:PROJECT:mode_spawn_failed` in the agent-id slot of an
  `AgentFailed` event. `ui_stream` synthesised a SessionUpdated
  for it, surfacing every retry as the same phantom agent.
- `ModeRuntimeState.in_flight` was only mutated in test fixtures.
  The live mode loop never incremented it, so every rule's
  `max_concurrency` guard was silently a no-op.
- A wedged spawn (e.g. checkout creation kept failing) tight-
  looped every 30s because the cooldown floor was a constant and
  the bd-4ff165 unclaim path returned the bead to the ready pool
  on every attempt.
- No operator-visible signal that burndown was wedged on a
  particular bead.

## After state

- New `EventType::ModeSpawnFailed` with subject
  `mode:<mode_name>` (mirroring `ModeAction`). Intentionally
  excluded from `ui_stream`'s AgentFailed-handling match arm so
  no SessionUpdated / phantom agent is synthesised.
- `mode_execution_loop` now increments `in_flight` by 1 right
  before calling `execute_action` and decrements by 1 on every
  outcome (Executed / Noop / Skipped / Failed). This gives a
  synchronous in-process gate that closes the spawn-issuance vs
  agent-registration race that the live-state predicate alone
  could not.
- `ModeRuntimeState` gains:
  - `consecutive_failures: HashMap<(mode, rule_index), u32>`
  - `failure_notified: HashMap<(mode, rule_index), bool>`
- `check_safety_controls` multiplies the configured cooldown by
  `2^min(failures, MODE_FAILURE_BACKOFF_MAX_DOUBLINGS=6)`, so a
  30 s base cooldown caps at 30 s × 64 = ~32 minutes. Counters
  reset on the next `Executed` or `Noop`.
- After `MODE_FAILURE_NOTIFY_THRESHOLD=3` consecutive failures a
  one-shot `Notification` feed event with
  `kind=mode_failure_streak` is published, including the failing
  mode, project, rule_index, and consecutive failure count.
- New constants exposed publicly so other layers (e.g. TUI status
  pill) can render the same thresholds.

## Diff summary

- Commits: `c05a15ef`
- Files touched:
  - `crates/caco-daemon/src/feed.rs` (+1 EventType variant)
  - `crates/caco-daemon/src/modes.rs` (in_flight + backoff +
    notification + 5 new tests)
- Tests: +5 / -0 / flipped 0 (modes::tests::* now 33 passing).
- Behavioural delta: feed no longer shows phantom
  `mode_spawn_failed` agent; concurrency cap actually holds;
  wedged rules back off exponentially and emit a one-shot
  operator notification.

## Validation

- `cargo test -p caco-daemon --lib modes::tests::` — 33 passed.
- `cargo test-small` workspace — all suites green
  (195 + 109 + 716 + 277 + 18 + 2780 + 45).

## Operator-takeaway

If burndown looks wedged, watch the feed for events with
`event_type=mode_failure_streak` and `kind=mode_failure_streak`
in the payload — that is the canonical signal that a rule has
failed three times in a row and is now backing off. The cooldown
displayed in the daemon log will include the backoff factor and
the failure count so you can tell at a glance whether the rule is
in the exponential-backoff regime or hitting the base cooldown.
