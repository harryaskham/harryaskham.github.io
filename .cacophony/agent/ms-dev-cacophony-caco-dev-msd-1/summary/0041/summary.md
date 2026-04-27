# Session summary — Ctrl-P persistent agents and test-small cleanup

## Goal

Make the TUI global Ctrl-P quick-open picker able to find individual persistent agent declarations and open their `PersistentAgentDetail` destinations directly, while clearing the unrelated broken-on-main test failures that blocked validation in this checkout.

## Bead(s)

- `bd-c0bb58` — TUI Ctrl-P should find individual persistent agents.
- `bd-10cc24` — `[broken-on-main] tests::shipped_profiles_html_matches_autogen_output failing` recurrence.
- `bd-00dbf4` — `[broken-on-main] playback::tests::write_to_pipe_broken_pipe_detected failing` recurrence.
- Reflection draft filed: `bd-a02866` — Clarify auto-claim behavior when a bead is dependency-blocked mid-claim.

## Before state

- Failing tests: `cargo test-small` initially failed on `supervisor_config_tests::enterprise_theme_is_registered_and_well_formed` until owned bead `bd-ddcb2a` landed; after that it exposed `tests::shipped_profiles_html_matches_autogen_output` drift and a recurrent `playback::tests::write_to_pipe_broken_pipe_detected` race.
- Relevant metrics: Ctrl-P quick-open searched regular worker agents and aggregate persistent-agent views, but not individual persistent declarations as detail destinations.
- Context: `bd-c0bb58` had an ownership collision while blocked on `bd-ddcb2a`; the implementation was preserved on `preserve/bd-c0bb58-msd1-8d2322155`, handed back, then replayed onto current `origin/main`.

## After state

- Failing tests: `cargo test-small` passed after the profile docs refresh and deterministic playback test update.
- Relevant metrics: focused `caco-tui` fuzzy-picker tests pass, including two new persistent-agent quick-open regressions; touched-crate `cargo clippy -p caco-tui --all-targets -- -D warnings` passes.
- Context: Ctrl-P now builds a `Persistent Agents` section from `TuiState::persistent_agents`, matches declaration name, persistent id, scope, project, node, state, and error text, and Enter opens `ContentPane::PersistentAgentDetail`.

## Diff summary

- Commits: `3a2a3d476` (`bd-c0bb58`), `64fe4117b` (`bd-00dbf4`), `903703dad` (`bd-10cc24`, already on current main after rebase).
- Files touched: `crates/caco-tui/src/views/fuzzy_picker.rs`, `crates/caco-tui/src/app.rs`, `crates/caco-tui/src/playback.rs`, `docs/profiles.html`.
- Tests: added 2 focused persistent-agent quick-open tests; refreshed generated profile docs; changed 1 flaky playback unit test fixture to wait for child exit before writing.
- Behavioural delta: operators can jump directly to individual persistent agent detail pages from Ctrl-P, and the validation lane no longer fails on stale profile docs or nondeterministic broken-pipe timing.

## Operator-takeaway

The user-visible improvement is small but high-leverage: persistent agents are now first-class quick-open targets instead of only reachable through aggregate views. The session also cleaned up two validation blockers, and the handoff friction around `bd-c0bb58` is captured as draft `bd-a02866` for later workflow hardening.
