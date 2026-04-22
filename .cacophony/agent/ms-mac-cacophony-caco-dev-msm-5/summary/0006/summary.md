# Session summary — bd-2b7a37: caco doctor surfaces stuck-past-stale agents

## Goal

Make manifestation C from bd-2b7a37 — "state=running but no fresh tool
activity past stale_after" — operator-visible without requiring a
separate `caco status` invocation. By-state counts show these agents
as `running` so dashboards look healthy and the wedge sneaks past
post-restart triage.

## Bead(s)

- `bd-2b7a37` — Post-restart agent handoff stuck pattern: now hitting
  persistents on ms-mac, not just workers (acceptance item 1)

## Before state

- `caco doctor` had no awareness of stuck agents.
- `caco status` already returned a `potentially_stuck` array
  (built in `handle_agents_summary`, bd-fca135) with idle, stale
  threshold, and `node_unreachable` annotation (bd-b1d4e9), but
  doctor was not wired to it.
- Operator had to run `caco status` separately to discover the
  wedge.

## After state

- New doctor area "lifecycle / stuck agents" between the lifecycle
  supervisor check (bd-4acdd7) and the cluster listener check.
- It pings `/api/v1/agents/summary` on the local daemon (when
  reachable), filters out `node_unreachable=true` entries
  (bd-b1d4e9), and emits:
  - **ok** when none are past stale.
  - **warning** when only workers are stuck (idle past
    `stale_timeout_secs`).
  - **error** when ANY persistent is stuck — the persistent layer
    is the cluster's coordination backbone per the bd-2b7a37
    escalation context, so this severity is intentional.
- Detail line: `<count> past stale: <id1>, <id2>, <id3>[, +N more]`
  so the operator can paste ids directly into
  `caco agent show` / `caco agent replace --fresh`.
- Recovery hint: tells the operator to inspect with `caco status`
  and consider `caco agent replace --fresh <id>` AFTER verifying
  the underlying process is dead.
- Existing doctor tests pass; new lifecycle area is silently
  ignored when the local daemon is not running (the test fixture
  case).
- `cargo clippy -p caco-cli --tests` clean.

## Diff summary

- Commit: `7fac1a44`
- Files touched: `crates/caco-cli/src/lib.rs` (+86 lines, single
  block inserted as section 4c).
- Tests: 0 added (this code path needs a running daemon to exercise;
  added inline doc explains the contract). Existing doctor tests
  pass.

## Out of scope (explicitly deferred)

bd-2b7a37 is a multi-acceptance-criteria parent bead. This change
satisfies only acceptance item 1 (surface the wedge to operators).
Remaining work, all separate bigger workstreams:

- Audit the daemon resume / ready-handoff state machine for paths
  that don't recover after a restart of either side of the
  handshake (manifestation A: `cause=resume_provider_ready_handoff_pending`).
- Add automated recreate-or-fail action when stuck > N min past
  stale_after (vs the current operator-driven `caco agent replace`).
- Re-evaluate stale_after defaults — 300s for workers and 1800s for
  persistents may not be appropriate post-restart.
- Diagnose whether stuck persistent processes are alive but the
  daemon channel is dead, or the process has died and the daemon
  hasn't noticed.

Did NOT close bd-2b7a37 — three acceptance items still open. Left
in `in_progress` with my assignment so a follow-up worker (or me
next session) can pick up the next slice.

## Operator-takeaway

After this lands, every `caco doctor` run during post-restart
triage will surface stuck agents with sample ids and a clear
warning/error, so the silent "running but wedged" mode bd-2b7a37
documented becomes one-line-visible in the standard health check.
