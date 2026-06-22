# bd-f900fa — caco-cli clippy-cleanup (gate-restore chunk #2)

## Goal
Clean caco-cli --all-targets clippy non-hot-file warnings so the gate-restore (bd-ff92cd, cargo clippy
--workspace) has fewer blockers. Sibling of bd-0e1697 (caco-daemon, landed 627586f38a).

## Before
caco-cli --all-targets clippy (fresh -D run bj-116e251f) failed with: loop_cmd.rs:81/83/85 (3x
manual_is_multiple_of) + lib.rs:131373 (assertions_on_constants).

## After
- Fixed the 3 loop_cmd.rs warnings (non-hot *_cmd.rs): format_interval_secs `secs % N == 0` ->
  `secs.is_multiple_of(N)` (clippy's own suggestion). Validated: fresh caco-cli clippy (bj-d36cb8fb,
  Compiling caco-cli) shows loop_cmd.rs GONE; only lib.rs:131373 remains.
- DEFERRED lib.rs:131373 (assertions_on_constants) — lib.rs command-tree is HOT (msd-1's active bd-5804a1
  lane), left for its owner per ctrl's scope (non-hot files only).

## Diff
- Code commit: c037130947 (defer the landed squash SHA to the reintegration receipt).
- crates/caco-cli/src/loop_cmd.rs: 3x `% N == 0` -> `.is_multiple_of(N)` in format_interval_secs.

## SPEC / gate-restore
Gate-restore prereq chunk #2 (bd-ff92cd). caco-tui is a separate later chunk (active offline-node work,
bd-ac0366). caco-cli non-hot-file clippy is now clean; the one hot lib.rs warning is owner-deferred.
