# Session summary — global bead dialog default project coverage

## Goal

Close out the global-view bead dialog default-project task by proving the operator-facing behavior directly: when the TUI opens the bead creation dialog from a fully global context, it should default to the daemon-configured `default_project` rather than whichever project sorts first alphabetically.

## Bead(s)

- `bd-f53223` — Opening beads dialog in global view should default to the configured default_project

## Before state

- Failing tests: none, but there was no explicit regression covering the exact global-view fallback path.
- Relevant metrics: runtime code in `crates/caco-tui/src/app.rs` and `crates/caco-tui/src/state/mod.rs` already hydrated `default_project` into `configured_default_project` and already used it as the final fallback when opening the bead-create dialog.
- Context: review showed the functional fix was already present on main; the missing piece was proof for the exact operator-reported scenario.

## After state

- Failing tests: none in the targeted bead-create coverage run.
- Relevant metrics: added one regression test for the fully-global bead dialog path; targeted `cargo test -p caco-tui bead_create_ -- --nocapture` passed with 26 tests.
- Context: the claimed bead is now covered by an explicit app-level test instead of relying on code inspection and adjacent current-project tests.

## Diff summary

- Commits: `190e7b64d`
- Files touched: `crates/caco-tui/src/app.rs`
- Tests: +1 / -0 / flipped 0
- Behavioural delta: no runtime behavior change; this session adds a regression test proving the existing fallback chooses `configured_default_project` in fully global TUI context.

## Operator-takeaway

This bug was already fixed in the TUI logic before this session; the important work here was turning that implicit fix into an explicit regression test for the exact global-view workflow Harry reported, so future refactors cannot silently reintroduce the alphabetical-project fallback.