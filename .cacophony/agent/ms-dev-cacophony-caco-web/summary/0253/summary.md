# Session summary — bd-71cdbb: test interleaved streaming_blocks rendering

## Goal

Close a real streaming test-coverage gap. The Pico renderer supports two
streaming shapes — separate `streaming_text`/`streaming_thinking` accumulators
AND an interleaved `streaming_blocks` array (a turn that thinks, answers, then
thinks again, rendered as ordered thinking/assistant bubbles). The native clients
use the interleaved form, but every existing mock had `streaming_blocks=[]`, so
that rendering path was never exercised. Add deterministic coverage.

## Bead(s)

- `bd-71cdbb` — caco-web Pico: end-to-end test for interleaved streaming_blocks rendering (thinking/text turn order)

## Before state

- Failing tests: none.
- `renderPicoSnapshot`'s interleaved-blocks branch (lines ~9987-9991) had no
  test; all mocks used the separate-accumulator path.

## After state

- Failing tests: none.
- New caco-web-observe subscenario (harness only; no product change):
  `mock_streaming_blocks_frames` carries `streaming_blocks = [thinking, text
  (**bold**), thinking]`; `PICO_STREAMING_BLOCKS_ASSERT_EVAL` asserts exactly
  three bubbles render in order (thinking, assistant, thinking) with the right
  text, and that the interleaved assistant block also goes through markdown
  (the `**bold**` renders as `<strong>` — confirming bd-947fb2 applies here too).
  Static-snapshot based, so no rAF/scroll timing fragility.
- caco-web-observe bin 12; live Chromium pico-pane run green: blocks subscenario
  returns `{count:3, kinds:[thinking,assistant,thinking], assistantBold:true}`.
  caco-web `--lib` 648; clippy clean.

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `crates/caco-web/src/bin/caco-web-observe.rs` — `mock_streaming_blocks_frames`,
    `run_pico_streaming_blocks_subscenario`, `PICO_STREAMING_BLOCKS_ASSERT_EVAL`.
- Tests: +1 live subscenario; +1 mock fixture.
- Behavioural delta: none (test-only).

## Operator-takeaway

The Pico transcript has two distinct streaming render paths and only one was
tested. Interleaved `streaming_blocks` (ordered thinking/text within a turn) is
the native-parity form; this locks its ordered rendering and confirms assistant
blocks on that path also get markdown. Static-snapshot streaming assertions are
the reliable way to test render order — live delta/rAF timing is too racy (see
bd-7e561e).
