# Session summary — bd-421072 slice 2c (final): delegated handlers for agent/bead table rows

## Goal

Complete the caco-web inline-onclick-with-user-data security sweep (bd-66018c /
bd-421072) by converting the last and most delicate sites — the agent and bead
table rows, which carried inline onclick + onkeydown (+ the agent row's
ondblclick-to-terminal) — to the safe data-* + delegated-handler pattern, while
preserving click navigation, Enter/Space keyboard activation, and double-click
to the agent terminal tab.

## Bead(s)

- `bd-421072` — caco-web: sweep dashboard inline-onclick-with-user-data for the
  bd-66018c single-quote breakout class. This slice completes it (close after
  landing).

## Before state

- Failing tests: none.
- Agent row: inline onclick=showAgentDetail + ondblclick=showAgentDetailTab(...,
  'agent-tty') + onkeydown(Enter/Space). Bead row: inline onclick=showBeadDetail
  + onkeydown. Same parser-decode-unsafe inline pattern as the rest of the sweep.

## After state

- Failing tests: none. caco-web lib tests: 674 passed, 0 failed (job tj-02bbcd79),
  including the rewritten table_rows_activate_on_enter_and_space_bd_7b1fcd.
- Agent row now uses data-show-agent-detail (click nav, reuses the slice-2a
  delegated click handler), data-agent-tty-id (dblclick to terminal, via the
  existing delegated dblclick handler), and keeps data-agent-id for arrow-key
  nav. Bead row uses data-show-bead-detail. The global keydown handler gained an
  Enter/Space branch that activates a focused role=button row via its data-show-*
  attr with preventDefault (WCAG 4.1.2, no Space-scroll).
- Live-dashboard validated: agent + bead row click navigates; Enter on a focused
  row navigates via the delegated keydown; double-click an agent row opens the
  agent terminal tab; console clean.

## Diff summary

- Code commit: 6b01fd051a (this checkout). Final landed squash SHA from the
  reintegration receipt.
- Files touched: crates/caco-web/static/app.js, crates/caco-web/src/tests.rs.
- The test table_rows_activate_on_enter_and_space_bd_7b1fcd was rewritten to
  assert the delegated keydown form (Enter+Space+preventDefault via data-show-*
  rows) instead of the removed inline onkeydown strings; the old-Enter-only-form
  guards were kept.
- Intentionally left: the safe-by-construction regex bead-ref onclicks (matched
  bd-[0-9a-f]+ only) and the feed-entry onkeydown (passes `this`, not user data).
- Behavioural delta: none intended. Tests: 1 rewritten, +0 / -0 net.

## Embedded artefacts

- screenshots/slice2c-row-nav.png — agents view after a delegated row activation,
  captured via headless chromium against the dev server serving this checkout's
  static.

## Operator-takeaway

bd-421072 is now complete: every inline onclick/onkeydown/ondblclick in caco-web
that interpolated a value into the handler string has been moved to data-*
attributes read by delegated document handlers, eliminating the bd-66018c
single-quote-breakout class across the dashboard (the one genuinely exploitable
sink, free-text copyToClipboard, was fixed first in slice 1). Validating on-disk
static edits required running the prebuilt caco-web-dev-server with --static-dir
because `caco web` serves embedded assets; that workflow is the main reusable
finding for future caco-web slices.
