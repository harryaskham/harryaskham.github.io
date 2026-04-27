# Session summary — test-small blockers: status, profiles, playback

## Goal

Clear the cluster's current test-small blockers while keeping the three issues distinct: TUI status telemetry label drift, shipped profiles HTML drift after the reintegration-mode change, and the recurring playback broken-pipe nondeterminism.

## Bead(s)

- `bd-56fd78` — test failure: caco-tui views::status::tests::render_with_telemetry_does_not_panic - cpu label must appear
- `bd-86ab31` — [broken-on-main] tests::shipped_profiles_html_matches_autogen_output failing after bd-9e4be4
- `bd-00dbf4` — [broken-on-main] playback::tests::write_to_pipe_broken_pipe_detected failing
- duplicate tracker `bd-79f259` was merged into `bd-00dbf4` and is not separate implementation work

## Before state

- Failing tests: `render_with_telemetry_does_not_panic` expected `cpu` after the UI row was renamed to `load`; `shipped_profiles_html_matches_autogen_output` detected `docs/profiles.html` drift for caco-web reintegration mode; `write_to_pipe_broken_pipe_detected` intermittently returned `Done` under `cargo test-small`.
- Relevant metrics: `cargo test-small` failed before the profile-docs and playback fixes; focused status and profiles tests reproduced the first two issues directly.
- Context: bd-00dbf4 had prior closed history for the playback race, so the duplicate bd-79f259 was consolidated back into that canonical tracker before landing.

## After state

- Failing tests: none in the final validation run.
- Relevant metrics: focused status test passes; focused shipped profile docs test passes; focused playback broken-pipe test passes; `cargo test-small` passes.
- Context: the status test now checks the intended `load` label, profile docs are regenerated, and the playback test uses an in-process writer that deterministically returns `BrokenPipe` instead of depending on OS pipe scheduling.

## Diff summary

- Commits: `c71ddf25f`, `5c21715e3`, `6e3485b57`; this summary commit follows them.
- Files touched: `crates/caco-tui/src/views/status.rs`, `docs/profiles.html`, `crates/caco-tui/src/playback.rs`
- Tests: updated two test fixtures/assertions and one generated docs snapshot; no production UI behavior change beyond test determinism.
- Behavioural delta: release validation no longer flakes/fails on these three known test-small blockers.

## Operator-takeaway

The test-small lane is green again for these issues. The playback duplicate bookkeeping was consolidated onto bd-00dbf4 before landing so there is one canonical recurrence tracker, not two competing implementation beads.
