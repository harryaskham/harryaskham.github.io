# Session summary — profile frontmatter composes: field

## Goal

Let a host profile (e.g. `dev`, `worker`) declare a default mixin
stack to prepend whenever it is referenced in an agent's profile
selection, so adding a new default mixin doesn't require editing
every agent definition that uses the host. Operator's motivating
case: get session-recording into every agent that uses worker.md /
dev.md by setting it once in the .md frontmatter instead of
duplicating it across every entry in cacophony_persistent.yaml.

## Bead(s)

- `bd-b1fdc8` — Profile frontmatter: add 'composes:' field to let a profile declare default mixin stack

## Before state

- `Profile` struct (caco-profile/src/model.rs) had `hook_mixins:` for
  hook composition but no `composes:` for profile-level composition.
- The only way to add a default mixin to dev/worker was to edit every
  agent definition's `profile: [...]` list — operators kept getting
  asked to "add session-recording to all the X agents", and the change
  was always a sweep across many YAML entries instead of a single
  edit to dev.md.
- Tests: no profile-composes tests existed.
- `cargo test-small`: clean.

## After state

- `Profile.composes: Option<Vec<String>>` added with rustdoc covering
  prepend semantics, transitive composition, dedup rule, cycle
  detection, and independence from `hook_mixins`.
- New helper `expand_composes(names, profile_dirs)` in
  `caco-daemon::agent::profile`:
  - Walks the input list, recursively prepending each profile's
    composes entries before the host slot.
  - Order-preserving de-duplication via `HashSet<String>` of seen
    names (first occurrence wins).
  - Cycle detection via `HashSet<String>` of in-progress names;
    revisiting an in-progress name returns
    `DaemonError::Other("profile composition cycle detected at '<name>' (bd-b1fdc8)")`.
  - Tolerates unknown profile names (downstream resolver remains the
    canonical source of the not-found error).
- `resolve_profile_selection_with_overrides` now expands `composes`
  for both `Single` and `Composite` selections. A `Single` whose
  profile declares composes is routed through the composite-resolve
  path so the prepended profiles are loaded, bridged, and merged via
  the existing rules.
- 8 new unit tests under `agent::tests`:
  `expand_composes_no_composes_returns_input`,
  `expand_composes_prepends_at_slot_in_order`,
  `expand_composes_dedup_explicit_mention`,
  `expand_composes_transitive_chain`,
  `expand_composes_detects_self_cycle`,
  `expand_composes_detects_indirect_cycle`,
  `expand_composes_tolerates_unknown_profile_names`,
  `expand_composes_diamond_dedup`.
- 1 new integration test `resolve_profile_selection_single_with_composes_expands_to_composite`
  that writes session-recording with `env: CACO_SESSION_RECORDING=1`,
  composes it from dev, selects `dev` as a `Single`, and asserts the
  resolved profile pulls session-recording's env value through.
- 7 Profile struct-literal sites updated with `composes: None` to
  satisfy the new field (caco-cli, caco-profile/lib.rs, compose.rs,
  bridge.rs).
- SPEC.md: new section 16.5.1a "Profile Composition (`composes:`)"
  documenting the contract.
- `cargo test -p caco-profile --lib`: 277 passed, 0 failed.
- `cargo test -p caco-daemon --lib expand_composes resolve_profile`:
  13 passed (8 expand + 5 resolve), 0 failed.
- `cargo test-small`: 195 + 107 + 716 + 277 + 18 + 2776 + 43 = 4132
  tests, 0 failed.

## Diff summary

- Commit: `9851e4fa`
- Files touched (8): `SPEC.md`,
  `crates/caco-profile/src/model.rs` (+ field & rustdoc),
  `crates/caco-profile/src/{lib,compose,bridge}.rs` (struct-literal
  updates),
  `crates/caco-cli/src/lib.rs` (struct-literal update),
  `crates/caco-daemon/src/agent/profile.rs` (expand_composes
  + Single/Composite expansion wiring),
  `crates/caco-daemon/src/agent/tests.rs` (+9 tests).
- Tests: +9 / -0 / flipped 0
- Behavioural delta: profiles that set `composes: [...]` now
  transparently prepend those names whenever they are referenced.
  Profiles without `composes:` retain the previous resolution
  semantics exactly (the only change to the no-composes path is the
  added Profile field, which is None and bypasses the new branch).

## Operator-takeaway

After this lands, you can drop `composes: [session-recording]` into
`.cacophony/profiles/dev.md` (or worker.md) once and every agent
that uses `dev` will pick it up automatically. The opposite
direction also works: keeping the explicit `session-recording` entry
in an agent's `profile: [...]` list is still safe — the dedup rule
prevents double-composition. If we ever want strict-mode
validate-time warnings for unknown composed profile names, that
plumbing should hook into `validate_config` rather than the runtime
resolver; out of scope for this bead, intentionally minimal.
