# Session summary — fix broken-on-main: 4 stale caco-web lib tests gating test-small (bd-2955c1)

## Goal

update-helper flagged a caco-web broken-on-main: 4 caco-web lib tests FAIL on true
main (test-small 705 passed / 4 failed), gating test-small for ALL cacophony-fast-tests
agents (caco-web is not excluded from the test-small alias). Unblock the fleet.

## Bead(s)

- `bd-2955c1` — broken-on-main: 4 caco-web lib tests stale on true main (claimed + fixed)

## Before state

- Failing tests (true main): pico_timestamps_and_pending_label_are_legible_bd_7c0e90,
  js_window_exports_are_referenced_somewhere_bd_0525ab,
  workspace_chat_pane_module_embedded_and_exposes_public_contract,
  workspace_chat_pane_group_scoped_direct_fanout_contract_bd_6d30f2.
- All 4 are stale-test drift from tonight's caco-web lands (code correct, tests assert old strings).

## After state

- Failing tests: none (`cargo test -p caco-web --lib`, tj-6f2b26a7, exit 0; full suite green).
- Fixes (all test-only — the implementations are correct):
  1. pico_timestamps: `.pico-time` asserted `--text-dim`; bd-bc7be7 correctly moved it to
     `--text-muted` for WCAG AA (—text-dim 2.9-3.1:1 failing AA; —text-muted 4.84-5.22:1). Test → --text-muted.
  2. js_window_exports: `window.picoCycleCollapseMode` is an app.js-only inline-onclick handler
     (same pattern as the allowlisted cacoPicoRetrySend). Added to the test allowlist.
  3. module_embedded: chat-pane send refactored to `const payload = { body, target }` (+ correlationId);
     needle updated from `body: JSON.stringify({ body, target })`.
  4. group_fanout: `postDirectMessage` gained a `correlationId` param; needle updated.

## Diff summary

- Code/content commit: pending final squash SHA from the reintegration receipt.
- Files touched: `crates/caco-web/src/tests.rs` (4 stale-test assertions updated).
- Tests: 0 added / 0 removed / 4 fixed (flipped red→green).
- Behavioural delta: none (test-only); test-small gate restored to green for the fleet.

## Operator-takeaway

Tonight's caco-web land velocity left 4 contract tests asserting pre-refactor strings
(a WCAG color move, an onclick-only window export, and two chat-pane send-shape changes),
silently reddening test-small for every cacophony-fast-tests agent. All fixes are test-only;
the implementations were already correct. Gate unblocked.
