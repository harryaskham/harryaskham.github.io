# Session summary — Async accept-and-background lifecycle endpoints, slices 1-2 (bd-14114e)

## Goal

P1: the synchronous agent lifecycle HTTP endpoints (refresh/resume/recreate)
run the slow Pi runtime relaunch under the 120s lifecycle timeout, so live
reloads transport-time-out AND recovery of stuck persistents is blocked. Convert
them to accept-and-background (202 ACCEPTED + background relaunch + status-
reflected progress), like async `caco pi`. This session lands the two slices on
the stuck-persistent critical path; the other two handlers follow.

## Bead(s)

- `bd-14114e` — Agent lifecycle endpoints should be async accept-and-background
  (bug, P1; kept in_progress for the remaining 2 handlers)

## Before state

- Failing tests: none (pre-change).
- `handle_agent_refresh` ran `state.agents.restart()` synchronously; live Pi
  refreshes transport-timed-out and did not reload.
- `handle_persistent_recreate` ran `relaunch_persistent_agent_after_recreate()`
  synchronously; recreating a stuck persistent timed out, leaving it
  unrecoverable without a daemon restart (5 stuck persistents:
  doctor/narrator/transcript-bead-filer/msm-2/picasso-dev-2).

## After state

- Failing tests: none. Recreate lib tests 15/15 green; refresh reload-blocker
  test green; daemon builds clean.
- `handle_agent_refresh`: new `background` request flag (default true). After
  the synchronous fast preflight (resolve/forward/project/profile/workspace
  materialization + tmux-dead reload-blocker), the slow `restart()` runs in a
  tokio::spawn and the endpoint returns 202 ACCEPTED (runtime_reload.status=
  reloading + status/attach guidance). Records Failed on launch error so the
  bd-435a8b resume_ready_handoff_pending marker does not strand the agent.
- `handle_persistent_recreate`: after the synchronous preflight, the slow
  relaunch runs in a tokio::spawn and the endpoint returns 202 ACCEPTED
  (status=recreating). This is the canonical stuck-persistent recovery path
  (`caco persistent recreate`), confirmed by the bd-435a8b owner.
- Backgrounding is safe: resume_inner/restart hold the registry lock only
  per-transition (short scoped blocks), not across the launch await, so
  concurrent status reads stay responsive.

## Diff summary

- Code/content commits: ec28b6abb3 (refresh slice), ebead07958 (persistent-
  recreate slice), db1ac301be (test update). Final landed squash SHA from the
  reintegration receipt.
- Summary artefact commit: intentionally omitted.
- Files touched: `crates/caco-daemon/src/lib.rs` (2 handlers + request body +
  1 test updated to the async contract).
- Tests: 1 test updated to assert 202 + poll for background relaunch; recreate
  suite 15/15 green.
- Behavioural delta: refresh + persistent-recreate now return promptly and
  relaunch in the background instead of blocking up to 120s; backward-compatible
  (`background:false` keeps the legacy sync refresh path; the generic
  `/agents/{id}/recreate` handler is unchanged).

## Embedded artefacts

None.

## Operator-takeaway

The two lifecycle endpoints on the stuck-persistent critical path — `caco agent
refresh` (the literal reported timeout) and `caco persistent recreate` (the
recovery path for the 5 stuck persistents) — are now async accept-and-background,
so they no longer transport-time-out and recovery is unblocked. IMPORTANT: the
running daemon (1.2.1018) still has the old sync code; it must be UPDATED to the
new build before caco-ctrl runs the recreates (flagged by msm-1). bd-14114e is
kept in_progress for the remaining two handlers (handle_agent_resume
non-persistent path + handle_agent_recreate), which have heavier inline
sentinel/feed/bead-continuity bookkeeping and are NOT on the stuck-persistent
critical path. Co-owned with msm-1 (bd-435a8b reconcile/marker-clearing).
