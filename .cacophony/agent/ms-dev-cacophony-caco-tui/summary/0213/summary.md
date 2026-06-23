# Session summary — caco-tui speech_popup tests robust to row additions (bd-b95841)

## Goal

Eliminate the recurring caco-tui broken-on-main class where adding a setting row
silently drifts hard-coded magic row-count + activate_row index literals in the
speech_popup tests, re-blocking the `test-small` reint gate for the whole
cacophony-fast-tests fleet. This was filed as a reflect-session draft after
diagnosing the bd-475bce gate-block earlier this session, then taken once load
dropped enough to validate a code change.

## Bead(s)

- `bd-b95841` — caco-tui speech_popup tests hard-code magic row-counts that
  drift on every row add; refactor to count-by-label/derived (task, P3).
- Context: same class as the closed `bd-7b55dd` and `bd-475bce` (bd-6ab0e3
  added agent-DM rows; both prior fixes only re-hardcoded new magic numbers).

## Before state

- `views::speech_popup::tests` asserted bare magic literals: `row_count == 13`
  and `== 15`; six `activate_row(&mut s, N, 0)` calls with hard-coded indices
  (0/1/4/5/7/9). Every SettingRow add shifts all of them at once -> a silent
  broken-on-main that only surfaces when the gate runs against the merge commit.
- Failing-test message on drift was uninformative ("13 != 14").

## After state

- Added `row_labels(s)` + `row_index(s, label)` test helpers.
- `row_count_with_capabilities` / `row_count_with_local_device_routing` now
  assert the explicit ordered label set, so a drift fails naming WHICH row
  changed (and which test to update), not a bare number mismatch.
- The 6 activate_row tests look the target row up by label, so inserting a row
  above them no longer breaks them.
- Validated: `cargo test -p caco-tui --lib views::speech_popup::tests` =
  21 passed / 0 failed (real queued compile on daemon 1.2.1349; first attempt
  hit a daemon_restart_recovered retryable infra outcome from Harry's
  `caco update --restart`, retried clean).

## Diff summary

- Code commit: `fc63df4fdf` (final landed squash SHA from the reintegration
  receipt).
- Files touched: `crates/caco-tui/src/views/speech_popup.rs` (test module only).
- Tests: 0 added / 0 removed; 8 made drift-robust (2 row_count + 6 activate_row).
- Behavioural delta: none (test-only refactor).

## Operator-takeaway

Row-list UI tests that hard-code magic counts/indices are a recurring
gate-blocking trap: every row add silently breaks them and the failure message
hides which row drifted. Label-based assertions (assert the expected label set;
look rows up by label) make the tests self-adjusting and self-diagnosing. The
same pattern should be applied to any other caco-tui row-list test module still
using magic indices.
