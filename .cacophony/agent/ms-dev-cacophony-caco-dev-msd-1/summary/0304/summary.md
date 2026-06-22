# Session summary — bd-d6064f watchdog-c slice-1b (case b detector)

## Goal
Implement case (b) of the watchdog-c family (bd-d4b93b): escalate-only DETECTION of a
DAEMON-PARENTED git op hung far past its timeout — distinct from case (a)'s lock-keyed phase
wedge (landed slice-1a) and from the mode-1 ppid==1 orphan reaper. The op is still parented to
the LIVE daemon (ppid == daemon pid) but its runtime far exceeds the timeout that should have
killed it (the run_git_command_with_process_group_timeout Err-path-not-terminating root,
bd-3886e8 Part 1). Primary target: the `git fetch/push cacophony-state` transport (the 45s
STATE_SYNC_GIT_TIMEOUT seen running 17m30s; transcript-bead-filer + ctrl evidence).

## Bead(s)
- bd-d6064f (in_progress, mine) — slice-1b, this land.
- Parent bd-d4b93b (in_progress, mine) — slice-1a (case a) landed 1d1c47446d.
- Cross-links: bd-3886e8 Part 1 (the timeout-Err-path-not-terminating root, blocks the
  force-kill follow-on); bd-6c7298 (cacophony-state publication backlog; a hung op on this path
  plausibly contributes).

## Before state
A daemon-parented git op hung past its timeout (ppid==daemon, not ppid==1) was invisible: the
mode-1 orphan reaper only matches ppid==1, and the process-group SIGKILL never fired, so the op
lingered for many minutes saturating git/SSH with no surfaced signal.

## After state (slice-1b — case (b) detection + escalate-only surfacing)
- Pure predicate `daemon_parented_hung_git_op_should_escalate(args, ppid, elapsed, daemon_pid,
  timeout, multiple)` = ppid==daemon_pid AND git exe AND cacophony-state fetch/push path AND
  elapsed >= timeout*4 (180s for the 45s state-sync bound). Conservative for escalate-only
  calibration; never force-kills.
- Best-effort Linux-primary enumeration `daemon_parented_git_procs` via
  `ps --ppid <daemon_pid> -o etimes=,args=` (raw elapsed seconds); any ps error → empty, so a
  hung op is never INVENTED. (macOS etime-format parsing is a noted follow-on.)
- `detect_daemon_parented_hung_git_ops` filters with the predicate;
  `escalate_daemon_parented_hung_git_ops` runs it in the existing 15min direct-integration sweep
  and emits a deduped structured daemon-log escalation (aggregated by the log-monitor surface)
  for each NEW hung op — deduped by args signature so a persistent hang reports once, and cleared
  signatures re-escalate on recurrence. NEVER kills.
- STATE_SYNC_GIT_TIMEOUT made pub(crate) so the per-op timeout baseline is shared (no drift).
- 2 unit tests: a rejection test (ppid==1 orphan / other parent / main-fetch / non-git /
  under-threshold are NOT flagged) AND a POSITIVE test for the real 17m30s (1050s)
  daemon-parented cacophony-state fetch hang (ctrl-requested). slice-1a's 10 tests unaffected.
- Escalate-only / observational: a deduped daemon-log scan in an existing sweep; no kill, no
  reint-flow change.

## Diff summary
- crates/caco-daemon/src/reintegration.rs: the (b) predicate, enumeration, detect + escalate
  fns, dedup static, and the unit test.
- crates/caco-daemon/src/cacophony_state.rs: STATE_SYNC_GIT_TIMEOUT → pub(crate).
- crates/caco-daemon/src/lib.rs: wire escalate_daemon_parented_hung_git_ops into the 15min sweep.
(Final landed squash SHA: see the reintegration receipt.)

## Operator takeaway
A daemon-parented git op hung past its timeout on the cacophony-state path is no longer silent —
it raises a deduped daemon-log escalation (log-monitor → bead) instead of lingering invisibly.
Escalate-only: it never kills the op (force-kill needs the bd-3886e8 Part-1 fix and is the
deliberate follow-on). SCOPED NEXT INCREMENT (slice-1c): the richer proactive caco ops finding
surfacing BOTH case (a) wedges and case (b) hung-ops on the operator-visible ops surface
(bd-f6a0f5 detect+surface theme) — a caco-cli ops integration kept as its own reviewable
increment.
