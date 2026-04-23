# Session summary — narrator resume profile-cache fix (bd-75989e)

## Goal

Stop the helsinki-cacophony-narrator persistent agent's recurring
"resume returns success but state immediately re-reads as stopped /
or returns 500/timeout" flap. Root cause was a profile-cache
staleness window that left the persistent sentinel believing
narrator.md was missing, causing the resume path to either
short-circuit or run the spawn pipeline against a phantom-missing
profile and surface noisy 500/timeouts to the operator.

## Bead(s)

- `bd-75989e` — helsinki-cacophony-narrator persistent agent: 'agent
  resume' fails (daemon 500/timeout, no adoption) while 'agent
  recreate' succeeds — recurring flap, ~3h cadence

## Before state

- `update_profile_availability` ran only inside the periodic
  `profile_discovery_loop` (60s interval). A transient cache miss
  could leave a persistent agent's `profile_unavailable=true` flag
  set for up to a minute even after the file reappeared on disk.
- `handle_agent_resume` did not check the sentinel's profile
  availability before delegating to `agents.resume()`. When the
  flag was stale, the spawn pipeline produced a 500 or timed out
  after 30s — operator-visible as noise misdiagnosed as a
  supervision flap.
- No fast-path between "operator just issued resume" and "discovery
  loop noticed the file is back".

## After state

- `handle_agent_resume` now does an opportunistic refresh on every
  resume attempt for persistent agents:
  1. Calls `discover_available_profile_names(&state)` synchronously.
  2. Calls `update_profile_availability(...)` against the live
     declarations + sentinel.
  3. Re-reads the sentinel state. If `profile_unavailable` is still
     true, returns a structured 503 envelope with code
     `profile_unavailable` and a `missing_profiles` list — instead
     of letting the spawn pipeline produce a 500/timeout.
- This closes the 60s window for transient cache staleness without
  changing the periodic discovery loop's cadence.
- 3 passing tests:
  - `update_profile_availability_marks_and_clears` (existing)
  - `bd_75989e_opportunistic_refresh_clears_stale_unavailable_flag`
    — pins the new contract: a refresh after a transient cache miss
    clears the flag synchronously, no sleep
  - `bd_75989e_genuinely_missing_profile_remains_flagged_after_refresh`
    — pins the negative case: refresh against an empty profile set
    leaves the flag set so the handler can surface the clear error

## Diff summary

- Files touched:
  - `crates/caco-daemon/src/lib.rs` — opportunistic refresh + 503
    envelope in `handle_agent_resume`; +2 unit tests
- Tests: +2 / -0 / flipped 0
- Behavioural delta: persistent-agent resume now closes the
  profile-cache staleness window operationally (synchronous refresh
  per resume attempt) and turns the rare genuine-missing case into a
  clear 503 envelope instead of opaque 500/timeout.

## Operator-takeaway

Watch helsinki-cacophony-narrator over the next 6-9h (covers ~3 of
the historical flap windows). If a flap still occurs, the operator
should now see either (a) clean recovery (resume succeeds because
the synchronous refresh cleared the stale flag) or (b) a 503 with
`code: profile_unavailable` and the actual missing profile names —
which would point at a deeper cache layer (e.g. embedded-profile
build cache vs disk discovery) that this fix does not yet address.
The fix does not touch `adopt_live_session` or the resume code path
for already-running tmux sessions; if the flap is supervision-side
(profile is fine, but reconciler races resume), this fix won't
catch it and a follow-up bead will be needed.
