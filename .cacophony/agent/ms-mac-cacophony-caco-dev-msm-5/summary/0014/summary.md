# Session summary 0014 — bd-732406 slice 1: post-restart tmux survival detection

## Goal

bd-732406 wants persistent agents whose tmux survived a daemon restart
to re-attach instead of respawn. Slice 1 is the cheapest, lowest-risk
first step: detect the situation and log a structured warning so
operators can see it; defer the actual reattach implementation to a
follow-up bead where the design can be reviewed.

## Bead(s)

- `bd-732406` slice 1 — this commit.
- Filed `bd-7a035f` for the actual reattach implementation.

## Before state

- `launch_persistent_agent`'s `cleanup_stale_persistent_agent` branch
  returning `Ok(None)` silently fell through to `state.agents.create`
  which spawned a fresh agent record + new tmux session.
- If the prior daemon left a tmux session alive (recorded in the
  sentinel's persisted `tmux_session` + `tmux_socket` fields), the
  fresh spawn produced a duplicate process. The orphan tmux was
  eventually reaped by `repair_stale_tmux_sessions` but the brief
  window allowed two processes to hold workspace locks.
- No log line surfaced the situation, so operators investigating
  bd-2b7a37 stuck-after-restart symptoms had to infer the cause.

## After state

- In the `Ok(None)` branch of `cleanup_stale_persistent_agent`, the
  daemon now consults the sentinel's recorded `tmux_session` +
  `tmux_socket` (which persist across daemon restarts via on-disk
  sentinel state).
- If both are recorded AND `crate::agent::verify_tmux_alive_pub_on`
  says the tmux session is still alive, a structured `bd-732406`
  warning is logged that explicitly names the persistent_id, decl
  name, session, and socket. This makes the post-restart wedge
  immediately visible in daemon logs.
- No behavioural change to the spawn path itself — the fresh agent
  record is still created. The follow-up bead (`bd-7a035f`) handles
  the actual reattach.

## Diff summary

- Commit: `97815142`.
- Files: `crates/caco-daemon/src/lib.rs` (+32).
- Tests: none added (logging-only path, deferred reattach gets test
  coverage in bd-7a035f).
- `cargo build -p caco-daemon`: clean.
- `cargo clippy -p caco-daemon`: clean.

## Out of scope (deferred to bd-7a035f)

- Actually skipping the spawn and reattaching to the surviving tmux.
- Reconstructing in-memory AgentInfo bound to the surviving tmux.
- Verifying the process inside tmux is the right binary (not just a
  leftover shell).
- Workspace-lock / fd state re-acquisition concerns.

## Operator-takeaway

After the next daemon restart on a node that hosts persistents,
grep the daemon log for `bd-732406` to see whether any persistent's
tmux survived the restart while the in-memory record was lost. Each
such line names the surviving session — currently you can manually
`caco agent recreate` to clean it up; bd-7a035f will eventually do
this automatically.
