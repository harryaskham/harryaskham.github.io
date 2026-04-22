# Session summary — bd-fbfff8: stop pi-self-nudge from freezing when agent_end is missed

## Goal

Diagnose and fix the "self-nudge stuck at armed, never verified"
symptom on persistent Pi agents.  Operator had to manually retype
"speak an update" every few minutes to wake the agent because the
periodic self-nudge silently stopped firing.

## Bead(s)

- `bd-fbfff8` — pi-self-nudge not firing for config-helper persistent agent — startup verification stuck at armed, never reaches verified (P2, bug)

## Before state

- Failing tests: none — all pre-existing self-nudge tests passed.
  The bug was a latent runtime freeze, not a test regression.
- Two silent freeze paths in
  `.cacophony/pi/self-nudge/extensions/caco-self-nudge.mjs`:
  1. `if (agentBusy && IDLE_ONLY) return;` — every tick exited.  If
     Pi emitted `agent_start` but dropped/deferred `agent_end`
     (compaction, lifecycle quirks), the scheduler skipped forever.
  2. `if (agentBusy && followUpQueuedWhileBusy) return;` — once set
     true on a busy-followUp, only `agent_end` could clear it.  Same
     dropped-event sensitivity.
- `lastTickStartedAt` was tracked but never inspected for staleness.

## After state

- `lastTickStartedAt`/`agentBusySince`/`consecutiveIdleSkips` now
  drive a self-healing scheduler:
  - `STUCK_BUSY_AUTOCLEAR_MS = 15 * 60 * 1000` — if `agentBusy` has
    been true for 15+ minutes without an `agent_end`, auto-clear and
    record a `stuck_busy_autoclear` runtime warning.
  - `MAX_IDLE_SKIP_BEFORE_FORCE = 3` — after 3 consecutive idle-skips,
    force a `deliverAs: "followUp"` nudge.  followUp queues without
    disrupting an active turn.
  - `agent_end` resets both counters so healthy cycles behave
    identically to before.
- 6 unit tests pass (3 existing + 3 new):
  - `skips_ticks_while_agent_is_busy_under_idle_only`
  - `forces_a_followup_after_max_idle_skip_before_force` (bd-fbfff8)
  - `agent_end_resets_the_consecutive_idle_skip_counter` (bd-fbfff8)
- `cargo test-small` — 4139 passed, 0 failed.

## Diff summary

- Commits: 7b2af554
- Files touched:
  - `.cacophony/pi/self-nudge/extensions/caco-self-nudge.mjs`
    (+38 lines of recovery logic, +2 module-level counters)
  - `.cacophony/pi/self-nudge/extensions/caco-self-nudge.test.mjs`
    (+3 regression tests, ~150 lines)
- Tests: +3 / -0 / flipped 0
- Behavioural delta: a missed `agent_end` event no longer freezes
  the self-nudge wake path indefinitely.  Worst case is now a
  ~15-minute pause + auto-recovery, not silent permanent freeze.

## Operator-takeaway

If a persistent agent's `caco msg speak` cadence ever drops to zero
again, the new history entries `periodic_self_nudge_skipped` and
`stuck_busy_autoclear` will tell you exactly what's happening.  If
you see `forcedFollowUpAfterStuckBusy: true` on a `periodic_self_nudge`
history entry, the recovery path fired — the runtime missed an
`agent_end` lifecycle event and we self-healed.  Repeated occurrences
of `stuck_busy_autoclear` in one session are the signal to look at the
runtime's lifecycle event delivery; the self-nudge layer is now
robust to that class of bug.
