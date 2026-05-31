# Session summary — Auto-reattach for spurious tmux attach detaches (bd-9aa8bc)

## Goal

Fix an operator-reported P1 regression where local agent tmux attachments
randomly detach mid-typing and never reattach, instead dropping the operator
into the "another attachment for this agent" dialog. The bead suspected the
recent CSI-u keyboard-encoding change (bd-1abfdc); this session set out to
confirm or refute that and deliver an operator-facing fix.

## Bead(s)

- `bd-9aa8bc` — Regression: local agent tmux attach randomly detaches while typing and never reattaches

## Before state

- Failing tests: none introduced; pre-existing red test-small being handled separately by aur-3.
- Behaviour: when the operator's actively-attached unified PTY (`tmux attach-session`)
  exited unexpectedly on a still-live agent, the TUI cleared input attachment,
  dropped to read-only preview, and showed the blocking "session kicked /
  another attachment" modal. Operator perceived this as "randomly detaches and
  never reattaches".
- Suspected cause per bead: CSI-u / enhanced-keyboard PTY byte encoding.

## After state

- Failing tests: none.
- Empirical finding: fed the exact bd-1abfdc sequences (CSI-u modified Enter
  `\x1b[13;Nu`, BackTab `\x1b[Z`, alt-prefixed `\x1b\x1b[Z`) into a real
  `tmux attach-session` client via a pty — none detach the client. The
  keyboard-encoding hypothesis is disproven for default tmux. Operator clue
  confirmed the real symptom is the session-kicked / PTY-exit path.
- Fix: when the operator was actively attached and the unified PTY exits
  unexpectedly (not self-initiated) on a non-terminal agent, the TUI now
  transparently re-establishes the authoritative attachment (bounded
  auto-reattach) instead of dropping to the dead-end modal. Bounded to 3
  attempts per agent within a 30s sliding window; on budget exhaustion it
  falls back to the existing informational modal so a genuinely-gone session
  cannot loop.

## Diff summary

- Code/content commit: pending final squash SHA from reintegration receipt.
- Files touched:
  - `crates/caco-tui/src/app.rs` — new `auto_reattach_attempts` state field;
    `try_auto_reattach_after_unexpected_exit` + pure `auto_reattach_budget_allows`
    helpers with `AUTO_REATTACH_MAX_ATTEMPTS`/`AUTO_REATTACH_WINDOW` consts;
    wired into the unexpected-PTY-exit branch to auto-reattach before falling
    back to the kicked modal; new unit test
    `auto_reattach_budget_is_bounded_and_window_resets_bd_9aa8bc`.
  - `crates/caco-tui/src/pty.rs` — regression test
    `key_to_bytes_plain_typing_is_literal_and_detach_safe_bd_9aa8bc` locking the
    invariant that plain typing keys stay literal (no ESC/CSI, no tmux prefix byte).
- Tests: +2 unit tests; existing key_to_bytes / session_kicked / attach tests still green.
- Behavioural delta: unexpected detach of an active attachment on a live agent
  now self-heals via bounded auto-reattach instead of a dead-end modal.

## Operator-takeaway

The "random detach" was NOT the new keyboard encoding (empirically disproven) —
it is the unified-PTY unexpected-exit path that dumped the operator into the
"another attachment" modal. The fix makes the operator's attachment
authoritative again by auto-reattaching, bounded so a truly-dead session still
surfaces the modal. The exact trigger that makes the `tmux attach-session`
client exit (another client, session churn, etc.) is still worth pinning down,
but the operator-facing resilience gap is now closed.
