# Session summary — router/narrator observer profile conflict

## Goal

Implement `bd-8a4683`: router and narrator persistents were blocked during startup by composite profile conflicts between the worker/endless lifecycle (`reintegration.mode=direct`) and observer roles (`reintegration.mode=none`).

## Bead(s)

- `bd-8a4683` — Router and narrator persistents blocked by reintegration.mode profile conflict

## Changes

- Updated `.cacophony/config.yaml` so the node-scoped `router` persistent on `ms-mac` imports `agents/persistent-observer.yaml` instead of `agents/persistent.yaml`.
  - This preserves Pi runtime helpers and the observer guard without composing the `endless` worker lifecycle that sets `reintegration.mode=direct`.
  - Added an inline comment documenting why router must not use the worker persistent snippet.
- Added regression coverage in `crates/caco-profile/src/lib.rs`:
  - `router_and_narrator_observer_stacks_avoid_reintegration_mode_conflict`
  - Asserts the router declaration imports observer defaults.
  - Asserts the narrator declaration keeps observer defaults.
  - Composes `persistent-observer + router` and `persistent-observer + narrator`, verifying both resolve to `reintegration.mode=none` with allowed mode `none`.

## Validation

- `rustfmt --edition 2021 --check --config skip_children=true crates/caco-profile/src/lib.rs` — passed.
- `cargo test -p caco-profile router_and_narrator_observer_stacks_avoid_reintegration_mode_conflict -- --test-threads=1` — passed.
- `caco config validate --config .cacophony/config.yaml` — passed with existing non-fatal warnings.
- `git diff --check` — passed.

## Coordination

- Claimed `bd-8a4683` only after local board status was strict-green and `caco bd claim` succeeded.
- Avoided claiming `bd-39f452` because it is assigned to `ms-dev-cacophony-project-health`.
