## Goal

Cut cargo test-small wall time by removing dead iterations from two
flat-sidebar scroll tests in caco-tui. Found via collab-mode tier 4
perf sweep after the operator's nudge to "continue toward your goal".

## Bead(s)

- `bd-ba55d7` — TUI flat-sidebar scroll tests dominate cargo test-small
  (~55s saved). Filed during this session via collab-mode tier 4 sweep.

## Before state

- `flat_sidebar_scroll_still_reaches_bottom_after_resize` showed the
  cargo "has been running for over 60 seconds" warning on every
  test-small invocation.
- caco-tui --lib suite ran in ~34s in test-small (was the second-slowest
  runner after caco-daemon's 35s compile-check).
- Both tests did `app.nav.rows.len() * 2` (≈160) iterations of
  `handle_mouse(ScrollDown) + terminal.draw(render)` even after the
  scroll had already pinned at the bottom — every excess iteration
  rendered the full ratatui frame for no behavioural coverage.

## After state

- caco-tui --lib suite now runs in ~10s in test-small.
- Both targeted tests, run together: 10.04s (was >60s for the resize
  test alone).
- Test intent preserved: each test scrolls until `scroll_offset ==
  expected_max`, then performs 5 additional scroll/render iterations to
  prove idempotence (scrolling past bottom doesn't unstick the
  viewport). Same assertions as before.

## Diff summary

- `crates/caco-tui/src/app.rs` (3 hunks):
  - `flat_sidebar_scroll_reaches_bottom_across_repeated_renders`:
    main scroll loop breaks once `scroll_offset == expected_max`; new
    inner 5-iteration loop preserves the idempotence check.
  - `flat_sidebar_scroll_still_reaches_bottom_after_resize`: same
    pattern applied to both the tall-viewport precondition phase and
    the short-viewport reach phase.
- No production code changed; this is a test-only perf fix.
- cargo test-small: 297 passed (caco-tui), all green workspace-wide.

## Operator-takeaway

cargo test-small is now ~24s faster per invocation. That's per-agent,
per-validation-cycle savings across the whole fleet. The bead pattern
("loop until terminal state, then a small idempotence check") is worth
copy-pasting wherever a UI test does N redraws to "make sure" rather
than asserting termination directly — there are likely more of these in
caco-web and caco-tui.
