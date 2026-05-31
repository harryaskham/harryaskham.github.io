# Session summary — Forward all keys except Ctrl-G to PTY when attached (bd-4593dc)

## Goal

Fix the operator-reported P1 where typing or pasting into an attached agent
misbehaves: typing `{`/`}` flips TUI tabs instead of reaching the agent, and
pasting (middle-click / Ctrl+Shift+V delivered as individual key events)
containing braces flips tabs, moves the visible pane off the agent, and detaches
with an "another attachment" popup.

## Bead(s)

- `bd-4593dc` — Fix tmux agent attach disconnection on pasted text
- (corrects `bd-3f2eb7`; complements msm-1's `bd-fd2848` tmux send_text buffer-paste fix)

## Before state

- `handle_key` ran the `{`/`}` tab-cycle shortcut (`handle_agent_detail_inner_tab_shortcut`,
  bd-3f2eb7) and the STT `o` doctor shortcut BEFORE the attached-mode forward
  blocks, so those printable keys were stolen even while a tmux/shell/SSH PTY
  owned input. SPEC.md explicitly codified the bd-3f2eb7 "braces switch tabs even
  when attached" contract.
- Operator (Harry) confirmed: cannot type a curly brace into an attached agent;
  it is interpreted as a tab-change shortcut. Pastes hit the same handlers.

## After state

- New `input_forwarded_to_pty()` gate; the pre-forward `{`/`}` and STT `o`
  handlers are skipped while attached, so the TUI captures only the Ctrl-G
  detach chord and forwards every other key to the agent PTY. To switch inner
  tabs while attached, detach first with Ctrl-G.
- SPEC.md §TUI keyboard contract updated to the corrected behaviour.
- Rebased onto main including msm-1's bd-fd2848.

## Diff summary

- Code/content commit(s): the production handle_key fix (committed pre-rebase)
  plus this SPEC/test commit; final landed squash SHA from the reintegration receipt.
- Files touched: `crates/caco-tui/src/app.rs` (gate + two guards + tests),
  `SPEC.md` (keyboard contract).
- Tests: +1 gate test (`input_forwarded_to_pty_gates_attached_modes_bd_4593dc`);
  rewrote the stale bd-3f2eb7 test into
  `agent_detail_braces_forward_to_pty_while_attached_bd_4593dc` asserting the new
  contract; handle_key (26), agent_detail, attach suites + clippy green.
- Behavioural delta: printable keys (incl. `{`/`}`/`o`) and key-event pastes now
  reach the attached agent instead of triggering TUI shortcuts/detaches.

## Operator-takeaway

The "paste disconnects tmux attach" and "can't type braces" symptoms shared one
root cause: TUI shortcut chords that overlap printable characters were captured
before the attach-forward path. The contract is now: while attached, the TUI
captures only Ctrl-G; everything else forwards to the agent. This pairs with
msm-1's tmux send_text buffer-paste fix for the full paste story. Needs operator
interactive confirmation that brace typing and paste are now clean.
