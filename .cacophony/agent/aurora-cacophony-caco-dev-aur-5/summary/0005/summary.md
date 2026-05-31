# Session summary — Bound synchronous stop/teardown for stuck-starting persistents (bd-4a6aad + bd-495880)

## Goal

Follow-up to bd-14114e: on daemon v1.2.1019 (which has the bd-14114e async
relaunch handlers), `caco agent restart/recreate/stop` against a persistent
stuck in `starting` STILL failed at the transport layer, leaving picasso-dev-2
unrecoverable. Root-cause and fix the residual so stuck-starting persistents are
recoverable via the CLI.

## Bead(s)

- `bd-4a6aad` — restart/recreate/stop still block on stuck-starting persistents
  (async fix gap), P2 bug.
- `bd-495880` — [broken-on-main] agent_stop_rejects_self_stop returns 404 instead
  of 403 (folded into this work because the fix lives in the same handler).

## Before state

- Failing tests: agent_stop_rejects_self_stop (broken-on-main, verified on a clean
  origin/main worktree: 0 passed).
- bd-14114e backgrounded the RELAUNCH, but stop/restart/recreate still ran the
  SYNCHRONOUS stop()/teardown first. On a stuck-`starting` agent, stop()'s
  unbounded work (tmux teardown on a wedged session + dirty-work checkpoint git
  ops, no timeout wrappers in stop()) hangs past the lifecycle transport timeout,
  so /stop, /restart, /recreate all transport-fail and the agent is unrecoverable.
- handle_agent_stop returned a misleading 404 for a self-stop of a nonexistent
  target because the resolve/not-found path ran before the self-stop 403 guard.

## After state

- Failing tests: none. agent_stop suite 6/6 (incl. the now-passing
  agent_stop_rejects_self_stop and the new bd-495880 + bd-4a6aad regression
  tests); recreate suite 15/15; caco-daemon clippy clean in the changed regions.
- handle_agent_stop: state.agents.stop() wrapped in a bounded timeout
  (effective_agent_lifecycle_lookup_timeout). On timeout, a background completer
  finishes the drain + sentinel operator_stop (or records Failed on error) and
  the handler returns 202 ACCEPTED {status:stopping}. Healthy fast path unchanged.
- handle_agent_restart: background flag (default true) + local-known precheck;
  locally-known agents background restart() and return 202 {status:restarting}
  (Failed on error). The not-found -> remote-forward path stays synchronous.
- handle_agent_recreate Step 2: pre-discard stop() bounded with a timeout; on
  timeout, continue to discard + the already-backgrounded relaunch.
- bd-495880: self-stop 403 guard moved ABOVE resolve/not-found so a self-stop is
  rejected by identity even when the target is not a live agent.

## Diff summary

- Code commits: 67f31a9768 (handlers + self-stop guard), plus a follow-up commit
  for regression tests + set_state warning fixes. Final landed squash SHA from
  the reintegration receipt.
- Summary artefact commit: intentionally omitted.
- Files touched: crates/caco-daemon/src/lib.rs (3 handlers + 1 guard reorder +
  2 new tests).
- Tests: +2 (bd-495880 self-stop, bd-4a6aad restart-unknown); 1 broken-on-main
  test flipped to passing. agent_stop 6/6, recreate 15/15.
- Behavioural delta: stuck-starting persistents are now recoverable (stop/restart
  return promptly + drain/relaunch in background); healthy fast paths and the
  not-found/forward contract are unchanged; self-stop now 403 by identity.

## Embedded artefacts

None.

## Operator-takeaway

The residual that left picasso-dev-2 unrecoverable was NOT in the relaunch (which
bd-14114e already backgrounded) but in the SYNCHRONOUS stop()/teardown that runs
first — its unbounded tmux/git work hangs on a wedged `starting` agent. This
bounds that stop on all three lifecycle handlers so a stuck agent returns
promptly and drains in the background. Once the daemon is updated to a build
including this, `caco agent recreate/restart/stop` will recover stuck-starting
persistents. A pre-existing broken-on-main self-stop guard-ordering bug
(bd-495880) was folded in since it lived in the same handler. The deferred
non-persistent resume conversion (bd-1cfc19) is still open and mine for next.
