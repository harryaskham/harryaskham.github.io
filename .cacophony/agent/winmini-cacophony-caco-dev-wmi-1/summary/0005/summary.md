# Session summary — Reattach persistent agents to surviving tmux (bd-7a035f)

## Goal

bd-7a035f (follow-up to bd-732406): when a persistent agent's tmux
session survives a daemon restart but the in-memory inventory has
lost the agent record, reattach the existing pane instead of
spawning a fresh agent record. Eliminates the duplicate-agent /
orphan-tmux window after a `caco daemon restart`.

## Bead(s)

- `bd-7a035f` — Implement reattach-instead-of-respawn for persistents
  whose tmux survived a daemon restart
- (parent: `bd-732406` — detection-only logging slice, already shipped)
- (related: `bd-2b7a37` — symptom bead for stuck-after-restart cases)

## Before state

- Failing tests: none. caco-daemon agent-tests: ~700 passing.
- Detection-only behaviour from bd-732406: when sentinel had a prior
  tmux_session+tmux_socket and that session was still alive, the
  daemon logged a structured warning and proceeded to spawn a fresh
  agent record. Two consequences:
    - Brief duplicate-agent window during which both the orphan
      process and the fresh spawn could compete for workspace locks.
    - Operator-visible churn — the sentinel's spoken_name / agent_id
      changed across the restart even though the pane had survived.

## After state

- Failing tests: none. +3 tests in caco-daemon (1 lib::tests, 2
  agent::tests).
- New: `AgentManager::reattach_persistent_agent` — re-inserts an
  AgentInfo from disk into the live inventory + persistent_id_index
  after re-verifying tmux liveness under the registry lock.
- New: `recover_agent_id_from_tmux` — parses
  `tmux -L <socket> show-environment -t <session> CACOPHONY_AGENT`
  to recover the prior agent_id from the surviving session.
- Wired into the bd-732406 branch of `launch_persistent_agent`:
  before logging the orphan-tmux warning, attempt reattach. On
  success, mark sentinel Running and `return Ok(recovered_agent_id)`.
  On any precondition failure (no env var, terminal record, missing
  agent.json, dead tmux), fall through to the existing
  bd-732406 log + fresh-spawn path.
- Refuses to resurrect terminal records (Discarded/Failed/Stopped/
  Completed) so operator-stopped persistents stay stopped.

## Diff summary

- Files touched:
  - `crates/caco-daemon/src/lib.rs` (+~115): bd-7a035f branch in
    persistent-launch path, new `recover_agent_id_from_tmux` helper,
    +1 unit test
  - `crates/caco-daemon/src/agent/lifecycle.rs` (+~70):
    `AgentManager::reattach_persistent_agent` method
  - `crates/caco-daemon/src/agent/tests.rs` (+~60): +2 negative-case
    tests
- Tests: +3 / -0 / flipped 0
- Behavioural delta:
    - When the recovery preconditions hold, no fresh agent record /
      no duplicate tmux session / no startup nudge — the surviving
      pane is silently re-adopted.
    - When any precondition fails (most common: agent.json was
      pruned, env var unset, or session genuinely dead), behaviour
      is identical to bd-732406 (log + spawn fresh).

## Embedded artefacts

(none — pure backend change)

## Operator-takeaway

After a `caco daemon restart`, persistent agents whose tmux pane
survived will now be silently re-adopted by the new daemon instead
of getting a duplicate agent record and orphan tmux. Watch the
daemon log for either:

    bd-7a035f: persistent <id> reattached to surviving tmux session
               '<session>' on socket '<socket>' as agent <id>
               (no respawn)

(the happy path) or:

    bd-7a035f: persistent <id> tmux session '<session>' on socket
               '<socket>' is alive but CACOPHONY_AGENT env var
               unrecoverable; falling through to fresh spawn

(reattach declined; bd-732406 log will follow).

The reattach path intentionally does NOT touch the running process
inside tmux (no startup nudge, no init.sh re-run). If an operator
wants to force a fresh spawn anyway, `caco agent recreate` still
takes precedence by going through the cleanup_stale path.

Note on bd-732406 base: main has since dropped the explicit
detection-log block in `Ok(None)`; the bd-7a035f branch now lives
inline at that callsite and is the sole reattach/log path. The
fallback fresh-spawn behaviour is unchanged.

Follow-up worth filing: an integration test that builds a real tmux
session, kills the daemon, restarts, and asserts reattach occurred.
The unit tests here cover only the precondition-failure branches;
the disk+tmux happy path needs the integration harness.
