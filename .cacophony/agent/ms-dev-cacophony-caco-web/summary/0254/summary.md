# Session summary — bd-1ad6f3: test failed-tool error rendering (operator-trust)

## Goal

Close an operator-trust test gap: a FAILED Pico tool (status Error) had no
coverage. Operators must be able to tell a failed tool from a successful one and
see the error output. The main scenario only exercised the success path.

## Bead(s)

- `bd-1ad6f3` — caco-web Pico: end-to-end test for failed-tool error rendering (operator-trust)

## Before state

- Failing tests: none.
- Only the `is_error:false` (Ok, green badge) tool path was tested; the Error
  (red `.pico-err` badge) path was unverified despite the render logic existing.

## After state

- Failing tests: none.
- New caco-web-observe subscenario (harness only; no product change):
  `mock_tool_error_frames` (Tool item status Error + error output) +
  `PICO_TOOL_ERROR_ASSERT_EVAL` asserting a `.pico-tool-status.pico-err` badge
  reading "Error" (NOT `.pico-ok`) and that the error output text is visible.
- caco-web-observe bin 12; live Chromium pico-pane run green: tool-error
  subscenario returns `{badgeText:"Error", hasErrClass:true, hasOkClass:false,
  showsErrorOutput:true}`. caco-web `--lib` 648; clippy clean.

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `crates/caco-web/src/bin/caco-web-observe.rs` — `mock_tool_error_frames`,
    `run_pico_tool_error_subscenario`, `PICO_TOOL_ERROR_ASSERT_EVAL`.
- Tests: +1 live subscenario; +1 mock fixture.
- Behavioural delta: none (test-only).

## Operator-takeaway

Tool failure visibility is operator-trust-critical and was untested. This locks
the distinct red Error badge + visible error output so a failed tool can never
silently look like a successful one in the Pico conversation.
