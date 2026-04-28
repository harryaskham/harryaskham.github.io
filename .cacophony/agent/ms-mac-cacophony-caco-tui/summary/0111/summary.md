# Session summary — Status not-running classification

## Goal

Continue TUI status-surface improvements by fixing service/supervisor health classifiers for explicit `not running` status strings.

## Bead(s)

- `bd-ce104e` — Status health classifiers should treat not running as stopped

## Before state

- Failing tests: none pre-existing; source inspection found a substring-classification bug.
- Context: `status_glyph` and `health_status_color` matched `running` by substring, so statuses like `not running` could render as running/green.

## After state

- Failing tests: none in focused validation.
- Context: `not running` and `not-running` classify as stopped/down before the `running` substring is considered. Matching remains case-insensitive.

## Diff summary

- Commits: `2d2ea4c98`
- Files touched: `crates/caco-tui/src/views/status.rs`
- Validation:
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-tui status_glyph_classifies_unhealthy_before_healthy_substring --lib`
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-tui health_status_color_classifies_problem_states_before_healthy_substring --lib`
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-tui status --lib`

## Operator-takeaway

The TUI status panels no longer display `not running` services as running/green; they now use stopped/down glyph and color semantics.
