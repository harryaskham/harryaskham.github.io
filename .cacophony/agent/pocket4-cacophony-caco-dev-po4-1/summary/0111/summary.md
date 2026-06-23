# Session summary — bd-eed5e0 (fix my bd-7e5dfe caco-web source-pin regression)

## Goal

Restore the caco-web part of the cacophony-fast-tests reint gate that MY earlier
bd-7e5dfe land broke: the group-chat fan-out source-pin test went red because I
added a correlation_id arg to the group send but did not update its source-pin
needle, and the echo-disabled gate let it land unvalidated.

## Bead(s)

- `bd-eed5e0` — [broken-on-main] workspace_chat_pane group_scoped_fanout test
  needle stale after bd-7e5dfe correlation_id (self-filed + owned, the regression
  author).

## Before state

- `workspace_chat_pane_group_scoped_direct_fanout_contract_bd_6d30f2` FAILED on
  test-small: bd-7e5dfe changed workspace-chat-pane.js:300 to
  `postDirectMessage(project, body, agentId, correlationId)` but the test still
  asserted the old 3-arg needle. caco-web is in test-small, so this gated ALL
  cacophony-fast-tests reints (alongside 3 unrelated caco-web failures owned by
  caco-web-md2-0).

## After state

- The test passes (tj-dfab2a1b, 1/1); `cargo check --workspace --tests` green
  (tj-e5b4b557). My 1-of-4 caco-web gate failure is cleared; caco-web-md2-0 owns
  the other 3 (js_window_exports, pico_timestamps, module contract) for the full
  gate-restore.

## Diff summary

- Code/content commit: ece7598f5 (final landed squash SHA from the receipt).
- Summary artefact commit: intentionally omitted.
- Files touched: crates/caco-web/src/tests.rs (1 line — source-pin needle).
- Tests: 0 added; 1 source-pin needle updated to match the intentional change.
- Behavioural delta: none (test-only).

## Operator-takeaway

This is the echo-disabled-gate escape vector in action: a source-pin test that a
real reint gate would have caught at reintegration time instead landed (bd-7e5dfe)
and only surfaced when someone ran test-small. The durable fix is re-enabling the
real gate (bd-ff92cd); until then, treat source-pin tests (caco-web/android JS/
content pins) as a known echo-gate blind spot and grep the pin tests when changing
pinned source.
