# Session summary — Fix stale caco-tui attach tests after auto-reattach (bd-904876)

## Goal

My bd-9aa8bc auto-reattach landing intentionally changed attach-exit behaviour,
which left three caco-tui tests red on main (they asserted the old
always-show-modal contract). This chunk updates those tests to assert the new
auto-reattach semantics so test-small goes green again.

## Bead(s)

- `bd-904876` — [broken-on-main] caco-tui: 3 attach/detach modal tests stale after auto-reattach landing (e25fa73f3a)
- (cause: `bd-9aa8bc` — auto-reattach on unexpected PTY exit)

## Before state

- Failing tests (caco-tui --lib): `pty_exit_running_agent_shows_modal`,
  `local_tmux_session_exit_shows_modal_when_attached`,
  `ssh_session_exit_auto_detaches` — all assert the old always-modal-on-exit path.
- test-small red on this lane.

## After state

- Failing tests: none. The three tests are rewritten/renamed to assert the new
  contract and pass (focused run: 5 passed).
- New contract asserted: while actively attached to a non-terminal (or
  unknown-in-map) agent, the FIRST unexpected PTY exit auto-reattaches (no
  modal, bounded 3 attempts / 30s); the session-kicked modal is only surfaced
  once the reattach budget is exhausted. Terminal-agent and preview-mode paths
  are unchanged.

## Diff summary

- Code/content commit: pending final squash SHA from reintegration receipt.
- Files touched: `crates/caco-tui/src/app.rs` (tests only).
- Tests: 3 rewritten/renamed (now two-phase: phase 1 auto-reattach + attempt
  recorded; phase 2 budget-exhausted modal), preserving modal coverage.
  Renamed to `*_auto_reattaches_*_bd_9aa8bc`.
- Behavioural delta: none (production code unchanged); test assertions now match
  the shipped auto-reattach contract.

## Operator-takeaway

This is the test-side follow-up to the bd-9aa8bc auto-reattach fix: the three
attach tests now encode the real contract (auto-reattach first, modal only after
the bounded budget is exhausted), so the new behaviour is locked and test-small
is green again. No production behaviour changed.
