# Session summary — Persistent operator-stop endpoint + recovery-row Stop (bd-f3482b)

## Goal

Implement bd-f3482b (my own follow-up from bd-598ca0 / audit bd-648239): the TUI
persistent recovery row (failed/stopped persistents with no live backing agent)
needed a control beyond Resume/Recreate. Now landable because aurora's daemon
restarted onto v1.2.1030 (d49f863ac), which includes the 2400s-gate fix
(c2255d9fa) — aurora is no longer on the 300s gate that blocked daemon landings.

## Scope decision

The audit asked for "Discard" there, but true runtime discard/teardown of a
config-declared persistent is semantically fraught (the declaration is just
re-reconciled). The meaningful operator action is **operator-stop**: halt a
thrashing failed `restart: always` persistent so it stops auto-restarting. The
daemon already had the `operator_stop` sentinel primitive; this exposes it.

## Landed

- **Daemon**: `POST /api/v1/persistent/<id>/stop` (`handle_persistent_stop`),
  mirroring `handle_persistent_start` — drives `operator_stop`, remote-forwards to
  the owning node, 404s unknown ids. Registered in both routers; `/stop` added to
  the `is_agent_lifecycle_write_path` cross-node proxy allowlist.
- **Client**: `stop_persistent_agent`.
- **State**: `PersistentAgentDisplayState::is_operator_stoppable()` — true when not
  already operator-stopped and restart policy is always/failure.
- **TUI recovery row**: Stop button (action `stop`) between Resume and Recreate,
  gated on `is_operator_stoppable`; dispatch -> `request_persistent_stop` with
  PersistentStopped/PersistentStopFailed result handling (op tracking, toast, log).

## Diff summary

- Code commits: pending final squash SHA from the reintegration receipt.
- Files: caco-daemon/src/lib.rs (handler + 2 route regs + allowlist + test),
  caco-tui/src/{client,event,state/mod,state/tests,app,views/button,views/agent_detail}.rs.
- Tests: 1 daemon allowlist test, 1 state predicate test, 4 button-builder tests,
  3 updated render tests. caco-daemon builds; caco-tui lib 4083 passed/0 failed;
  clippy -p caco-tui -p caco-daemon --lib clean.

## Operator-takeaway

A failed/stopped persistent agent's TUI recovery page now shows a **Stop** button
(when it could still auto-restart), so you can halt a thrashing `restart: always`
persistent directly from the pane instead of editing config. Resume/Recreate
unchanged. True declaration teardown remains a config-edit operation by design.

## Scoped out / follow-up

- Audit M2 (Stop/Discard for starting/retrying transient states) is now a small
  follow-up given the new endpoint + predicate.
- A real declaration-removal teardown, if ever wanted, needs a config-edit path,
  not a runtime button.
