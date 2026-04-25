# Session summary — build show empty-id test repair

## Goal

Fix the broken-on-main `dispatch_build_show_rejects_empty_id` unit test so it verifies the current implementation behavior rather than brittle source-code shape.

## Bead(s)

- `bd-7f6df9` — [broken-on-main] caco-cli dispatch_build_show_rejects_empty_id failing under full lib suite

## Before state

- `timeout 120 cargo test -p caco-cli --lib dispatch_build_show_rejects_empty_id -- --nocapture` failed.
- The test inspected `include_str!("lib.rs")` and looked for a literal error string inside the `dispatch_build_show` body.
- The dispatcher now delegates to `validate_non_empty_id`, so behavior was still correct but the test was coupled to an obsolete implementation shape.

## After state

- The test calls `dispatch_build_show(false, &flags, None)` directly with `--project cacophony` and `--id ""`.
- It asserts the returned error contains `--id must not be empty for caco build show`.
- `timeout 120 cargo test -p caco-cli --lib dispatch_build_show_rejects_empty_id -- --nocapture` passes.
- `timeout 120 cargo test-small` passes.

## Diff summary

- Commit: `9ae73df61`
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: rewired one existing test; no production behavior change.
- Behavioural delta: none for users; test now locks behavior instead of source layout.

## Operator-takeaway

The broken test was caused by a good refactor to the shared empty-ID validator. The repair keeps the gold-standard error contract while avoiding future false failures when implementation is factored differently.
