# Summary 0014 — bd-1d302a: profile composition disable_hooks opt-out

## Bead
bd-1d302a (P3, feature) — Profile composition `disable_hooks: [<mixin>:<phase>]`
to opt OUT of an inherited hook from a composed mixin. Pairs with the
composability cluster (bd-b1fdc8 composes, bd-f30a29 composes_well_with).

## Use case

Composing `dev + merge-queue + collab-mode` produces a stack of hooks.
A dev wants `collab-mode`'s comms wiring without `merge-queue`'s
`before_reintegration` gate (e.g. for fast experimental work). Today
the only escape is forking the merge-queue mixin or scaffolding a
new variant — friction for what should be a one-line opt-out.

## Design

YAML format (frontmatter or composition site):

```yaml
composes:
  - dev
  - merge-queue
  - collab-mode
disable_hooks:
  - merge-queue:before_reintegration
```

`disable_hooks` is a list of `"<mixin-profile-name>:<phase-name>"`
entries. During `compose_hooks` merging:
- Build the union of all profiles' `disable_hooks` entries.
- For each phase × source-profile combination, if
  `(profile.name, phase)` is in the disabled set, skip that
  profile's contribution to that phase.

Semantics:
- **Phase-precise**: disabling `merge-queue:before_reintegration`
  only drops merge-queue's `before_reintegration` hooks; its
  `on_start`, `on_complete`, etc., still apply.
- **Mixin-precise**: disabling `a:on_start` doesn't affect `b`'s
  `on_start` contributions.
- **Union across profiles**: any profile in the composition stack
  may contribute disable entries. They union.
- **Forward compatible**: malformed entries (no `:`, empty
  mixin/phase) and unknown mixin/phase names are silently ignored
  — won't break compositions when phases get renamed or mixins
  are reorganised.
- **Already-resolved on the composite**: composed profile's
  `disable_hooks` is `None` (consistent with `composes`,
  `hook_mixins` — already-applied fields aren't re-surfaced).

## Change

`crates/caco-profile/src/model.rs`:
- New `Profile::disable_hooks: Option<Vec<String>>` field, with
  full doc comment, YAML example, and phase-name reference list.

`crates/caco-profile/src/compose.rs`:
- `compose_hooks` rewritten to:
  - Build `HashSet<(mixin_name, phase_name)>` from union of all
    profiles' `disable_hooks`. Malformed entries dropped via
    `splitn(2, ':')` + empty checks.
  - `merge_phase` helper now takes `&disabled` and the phase name
    string; skips a profile's contribution when
    `(profile.name, phase_name)` is in the disabled set.
- Composite `Profile { ..., disable_hooks: None, ... }`.

`crates/caco-profile/src/bridge.rs`,
`crates/caco-profile/src/lib.rs`: filled in `disable_hooks: None`
in 4 test fixtures (test_profile, caco_worker_profile,
caco_worker_profile_with_hooks, minimal_profile).

`crates/caco-daemon/src/ui_stream.rs`: dropped 4 duplicate
`tmux_history_limit/size: None` lines that I added in summary 0013
which main has since landed itself; my drive-by became redundant.

## Tests

`crates/caco-profile/src/compose.rs::tests::`:

- `compose_disable_hooks_drops_specific_mixin_phase` — happy path:
  dev + merge-queue + collab-mode where collab-mode disables
  merge-queue:before_reintegration. on_start (both contributions)
  preserved; before_reintegration dropped.
- `compose_disable_hooks_only_targets_named_mixin` — disabling
  `a:before_reintegration` leaves `b:before_reintegration` intact.
- `compose_disable_hooks_unknown_mixin_or_phase_silently_ignored`
  — typos + malformed entries (`:foo`, `foo:`, `no_colon`,
  unknown mixin, unknown phase) don't break composition.
- `compose_disable_hooks_unions_across_profiles` — multiple
  profiles each contribute disable entries; both are honoured.

## Verification

- `cargo check --workspace` — clean across all crates.
- `cargo test -p caco-profile --lib compose::` — 35/35 green
  (4 new + 31 existing).
- `cargo test -p caco-daemon --lib compose` — 15/15 green
  (no breakage in daemon's profile-composition consumers).

Pre-existing test issues (msm-1/2 SIGABRT cluster + bd-7ef076
caco-tui breakage) prevent full `cargo test-small`; targeted
crates green.

## Operational impact

- Operators can compose richer profile stacks without forking
  mixins to remove a single hook.
- Ergonomic for collab-mode + merge-queue composition (the
  motivating example) where one wants the comms wiring without
  the gate.
- No backward compat impact: `disable_hooks` defaults to `None`
  (omitted from existing profiles → behaves exactly as before).
- Bridge output (claude/codex/pi) untouched — disable_hooks is
  resolved entirely at compose-time, never reaches the runtime.

## Deferred

- bd-1d302a's description proposed alternate sugar
  (`merge-queue!no-before-reintegration`); skipping for now —
  the `disable_hooks` list form is more readable and discoverable
  in YAML. Revisit if operators want shorthand.
- No CLI surface in this slice — `caco profile show` already
  renders the composed Profile, and disable_hooks doesn't
  surface on it (already-resolved). If we want to display
  *which* hooks were dropped at compose time, that's a separate
  diagnostics bead.

## Next

Reintegrate direct, close bd-1d302a, idle.
