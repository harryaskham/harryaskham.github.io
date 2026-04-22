# bd-e03445 — extend cacophony-fast-tests gate to compile-check daemon test targets

## Goal
Catch struct-shape changes that `cargo test-small` skips (because it
excludes caco-cli / caco-daemon / caco / caco-sidecar) at the
reintegration gate, before they reach main and break every peer's
next pull.

## Bead(s)
- bd-e03445 (P2 feature, test-user / discovered-via wave-15-overlap):
  `[merge-queue gate] extend cargo test-small to compile-check daemon
  --tests as well`. Pairs with bd-9ab2b6 (gate plumbing landed),
  bd-e5eec5 (stale-base re-check landed), bd-2c399b (full queue
  daemon, the proper fix), and bd-bf1e86 (the wave-15
  PersistentAgentDecl-goal change that motivated the bead).

## Before state
- `cacophony-fast-tests.md` set `check_command: cargo test --lib
  --workspace`. That command runs tests on workspace lib targets but
  never compiles the test fixtures inside the four crates excluded
  from the test-small alias.
- A struct-shape change like adding a field to `PersistentAgentDecl`
  passed the local gate and only failed for peers when they ran
  `cargo test` after pulling main.
- The fast-test-gate.sh hook (bd-9ab2b6) already exposes
  `CACO_REINTEGRATION_CHECK_CMD` as a separate compile-check step;
  the profile just wasn't pointing it at a useful command.

## After state
- `cacophony-fast-tests.md` now sets
  `check_command: cargo build --workspace --tests`. This compiles
  every test target in the workspace in ~30s warm without executing
  them, so any field-mismatch / API-shape regression in test fixtures
  surfaces at the gate.
- The profile prose now lists the three Cargo invocations explicitly
  (`test-small`, `build --tests`, `clippy`) so readers don't have to
  reverse-engineer the ordering from fast-test-gate.sh.
- The wave the new gate immediately surfaced is also fixed:
  `crates/caco-daemon/src/persistent.rs` had 74 `PersistentAgentDecl`
  literals carrying a duplicate `goal: None` line (introduced by a
  recent reintegration leaving conflict-resolution leftover):
      project: None,

      goal: None,         <-- duplicate

      depends_on_node: None,
  Mechanical removal across two indent levels (60 + 14 sites).
- Verified clean: `cargo build --workspace --tests` + `cargo clippy
  --workspace --all-targets -- -D warnings`.

## Diff summary
- `.cacophony/profiles/cacophony-fast-tests.md` (+15/-5):
  - `check_command` switched.
  - Description block + body prose updated to reflect the three-step
    gate and cite bd-9ab2b6 / bd-e03445.
- `crates/caco-daemon/src/persistent.rs` (-222 lines, +0):
  - Stripped 74 duplicate `goal: None` field literals plus their
    surrounding blank-line padding.

## Operator-takeaway
- Workers using the `cacophony-fast-tests` mixin will pay ~30s extra
  per reintegration for the new compile-check step. In return, the
  `goal: None`-style waves (~5–15 minutes of peer repair time each;
  bd-ee1696 reported 13+ in a session) should stop reaching main.
- Operators can still override per invocation:
  `CACO_REINTEGRATION_CHECK_CMD='cargo check --workspace --tests'`
  for an even cheaper compile pass, or empty string to disable.
- Pairs with bd-5b02d6's opt-in broken-on-main rebase-and-test gate
  on the CLI side. The two layers compose: this bead catches it
  pre-rebase; bd-5b02d6 catches what slips through after rebase.

## Tests
- `cargo build --workspace --tests` — clean.
- `cargo clippy --workspace --all-targets -- -D warnings` — clean.
- The profile change has no Rust test surface; behaviour is end-to-end
  validated by the wave the new gate just exposed and fixed.
