# Session summary — Fix TUI keystroke leak from attached agent panes (bd-cabaa1)

## Goal

Stop attached agent panes from leaking keystrokes into TUI control commands.
The operator reported that while attached to a remote agent pane, some
keystrokes were being interpreted as TUI commands (Ctrl+W etc.), closing panes
and changing layout. The contract is: while attached, the TUI must capture only
the Ctrl-G detach chord and forward every other key to the agent.

## Bead(s)

- `bd-cabaa1` — Fix TUI keypress handling in attached agent panes to prevent
  control interference (P0 bug, type bug).

## Before state

- Failing tests: none (the bug was uncovered behavior, not a failing test).
- `crates/caco-tui/src/app.rs::handle_key` runs `detach_if_target_not_visible()`
  near the top on every keypress (~line 11241). That guard auto-detaches a stale
  PTY attachment (agent/visible-pane mismatch, inner tab drifted off Attach, or
  shell no longer visible) by calling `detach_input_attachment()` and returning
  `()`. The triggering keystroke then continued through `handle_key` to the
  ungated TUI-command branches below (Ctrl+W hide-workspace ~12518 / tile resize
  ~12580, tab shortcuts). So a single key could BOTH auto-detach AND run a TUI
  command — the reported pane-closure / layout-change symptom.
- Established invariant: when `input_forwarded_to_pty()` is true, one of the
  per-mode dispatch blocks (tmux ~11277 / shell ~11296 / ssh ~11323) always
  captures+returns every non-Ctrl-G key. So the ONLY leak path was a detach that
  flips that gate false mid-handler.

## After state

- Failing tests: none.
- `detach_if_target_not_visible()` now returns `bool` (true when it detached
  the active input attachment this call). `handle_key` captures whether input
  was genuinely being forwarded BEFORE the guard runs
  (`was_forwarding_to_pty = self.input_forwarded_to_pty()`) and does
  `if self.detach_if_target_not_visible() && was_forwarding_to_pty { return; }`,
  swallowing the triggering keystroke only when a GENUINE attachment was torn
  down. A stale attachment that was not forwarding (e.g. ssh_attached set
  without a matching attached_pane_id) is still cleaned up but its key falls
  through to normal TUI handling, preserving bd-060e65 / bd-2f8efd. Ctrl-G
  detach and normal per-mode forwarding are unchanged.
- Focused caco-tui lib test run (queued tj-9d50ab30, exit 0): 16 tests passed
  including the 4 new bd-cabaa1 tests. First reintegration gate (tj-8d769901)
  caught that the initial unconditional swallow broke
  `stale_ssh_attached_falls_through_to_normal_keys`; the `was_forwarding_to_pty`
  gate reconciles both contracts.

## Diff summary

- Code/content commits: d0dbdf4a0 (initial fix) + the `was_forwarding_to_pty`
  refinement commit (`bd-cabaa1: only swallow on genuine-attachment detach`).
  Final landed squash SHA will come from the reintegration receipt.
- Files touched: `crates/caco-tui/src/app.rs` (~172 insertions, ~5 deletions).
- Behavioural delta: a keystroke that tears down a GENUINE PTY attachment via
  the stale-attachment guard is now swallowed instead of also executing a TUI
  command; stale (non-forwarding) attachments still fall through after cleanup.
  Ctrl-G detach and attached-pane key forwarding are unchanged.
- Tests: +4 regression tests (`ctrl_w_hides_workspace_when_not_attached_bd_cabaa1`
  control proving the hide-workspace command is reachable so the guard
  assertions are non-vacuous; `attached_inner_tab_drift_swallows_ctrl_w_no_workspace_hide_bd_cabaa1`
  the fix; `attached_visible_ctrl_w_forwarded_not_workspace_hide_bd_cabaa1`;
  `attached_visible_ctrl_g_still_detaches_bd_cabaa1`).

## Operator-takeaway

The attached-pane key-leak was not a missing gate on the TUI command branches —
those are already unreachable while `input_forwarded_to_pty()` is true because
the per-mode dispatch blocks capture+return first. The single hole was that the
top-of-handler stale-attachment guard could auto-detach mid-keypress, flipping
that gate to false so the SAME key then ran a TUI command. Swallowing the
detaching keystroke closes it. Validation was done at the deterministic
unit level driving the real `handle_key` dispatch (not a mock) rather than
injecting keys into live agents on this shared host, which would have disrupted
running work. Possible follow-up: investigate whether the guard ever
*spuriously* detaches during normal attached typing (transient workspace/nav
content mismatch); if so, that is a separate UX bug (unexpected detach), now
without the layout-damage side effect.
