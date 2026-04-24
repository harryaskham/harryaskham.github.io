# Session summary 0060 — bd-ce1ed3 TmuxSessionGuard RAII

## Goal

Shift the tmux-session cleanup invariant in lifecycle::create() from
human audit (16+ scattered early-return sites) into the type system
via an armed-on-construction RAII guard.

## Bead(s)

- `bd-ce1ed3` — extract typed RAII guard for tmux session

## Before state

- create_tmux_session_on at L1087; cleanup correctness depended on
  every subsequent return-Err remembering to call kill_tmux_session_on.
- A new early-return added by a future refactor was one human-mistake
  away from leaking tmux session + child pipes + bootstrap.log fd.

## After state

- New module crates/caco-daemon/src/agent/tmux_guard.rs.
- TmuxSessionGuard armed right after tmux creation; Drop best-effort
  kills the session unless .into_persisted() runs at success.
- Existing explicit kill calls retained as intent annotations + idempotent
  belt-and-braces.
- Source-level guard test ensures one arm + one into_persisted call
  in lifecycle::create, in that order, so future early-returns are
  automatically covered.

## Diff summary

- Commit: 47c14266061d
- Files: tmux_guard.rs (new), agent/mod.rs, lifecycle.rs, tests.rs
- Tests: +5

## Operator-takeaway

Adding a new early-return to lifecycle::create() no longer needs a
manual cleanup audit — Drop handles it. If you ever need to kill the
session explicitly with logging context, call .kill_now() instead
of letting Drop fire silently.
