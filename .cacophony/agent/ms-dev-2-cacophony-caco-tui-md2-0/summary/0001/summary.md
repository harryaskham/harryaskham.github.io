# Session summary — pico keyboard-input attach/routing fix (bd-7d472d)

## Goal

Fix the operator-reported P2 where keyboard input to pico agent panes did not
work in the caco-tui: pressing `i` to attach and then typing reached neither the
composer nor the pico session, even though the pico chat bubbles rendered
correctly. Controller (caco-ctrl) assigned this by ID during a burndown spike,
clearing the operator-action gate, with scope: land a headless code-fix of the
attach/key-routing seam (bd-e27320) plus a failing-context unit test, leave a
clear live-validation note, and do NOT close on headless evidence alone.

## Bead(s)

- `bd-7d472d` — [pico] caco-tui: cannot keyboard-type to pico agent panes (`i` attach / text input does not work; bubbles DO render). P2 bug, operator-action (controller-assigned by ID).

## Before state

- Failing tests: none (the bug had no regression test).
- `PicoPane::on_key` (picophony_pane.rs:249) was correct; the bug was upstream in
  app.rs routing. Both the early `i`-attach intercept (~11703) and the
  key-routing predicate `is_content_text_input_active` (~11800) gate on
  `current_pico_session_agent_id()` (app.rs:41777), which resolved the pico
  Session target ONLY from `current_content_pane()`. That can diverge from the
  nav-selected pico agent (the bd-e27320 "3 sources of truth" focus seam, e.g. a
  multi-tile focused tile lagging the nav selection). When it returned None,
  `i`-attach AND typing both silently failed — matching the symptom exactly.

## After state

- Failing tests: none.
- `current_pico_session_agent_id()` now falls back to the nav-selected agent when
  the focused content pane does not resolve a pico Session target, bounded to
  pico-agent nav nodes (the `is_pico` guard keeps chat / non-pico selections from
  ever resolving a pico target — no key-steal regression). Because both the
  attach intercept and the routing predicate flow through this one helper,
  resolving it here fixes attach and typing together.
- Validation (queued on shared host from the agent checkout):
  - `cargo check -p caco-tui --tests` → succeeded (exit 0).
  - `cargo test -p caco-tui pico_session` → 4 passed, 0 failed (new test + the
    bd-d57012 regression guard + 2 existing pico_session tests).
- NOT closed: needs operator live-kitty validation (press `i`, type, Enter →
  text reaches the pico agent on the operator's live TUI).

## Diff summary

- Code/content commit: pending final squash SHA from the reintegration receipt.
- Files touched: `crates/caco-tui/src/app.rs`.
- Reworked `current_pico_session_agent_id()` to add a nav-selected-agent fallback
  (bd-e27320 source-of-truth unification).
- Tests: +1 (`pico_session_target_resolves_via_nav_when_content_pane_diverges_bd_7d472d`),
  asserting the divergence resolves via nav, that an attached pane routes keys
  (`is_content_text_input_active`), plus a non-pico regression guard.
- Behavioural delta: pico `i`-attach + typing now work when the nav-selected pico
  agent's focused content pane diverges; no change for non-pico nav selections.

## Operator-takeaway

The pico keyboard-input bug was NOT in the composer (`PicoPane::on_key` was fine)
— it was a focus source-of-truth divergence (bd-e27320): the single helper that
gates BOTH `i`-attach and key routing resolved only from `current_content_pane`,
which can lag the nav selection in multi-tile/focus-shift cases, so attach and
typing failed together silently. The headless fix + unit test prove the routing
resolves; the remaining step is your live-kitty confirmation (press `i`, type,
Enter on a real pico agent) before this bead closes. If `i`+typing STILL fails
with the content pane already focused, the divergence is elsewhere — verify
`agent_type=="pico"` for the affected agents.
