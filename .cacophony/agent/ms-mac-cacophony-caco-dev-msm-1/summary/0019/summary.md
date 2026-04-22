# Session 0019 — bd-4b2a82 (caco agent introspect --show hooks)

## Goal

bd-5d2d83 slice 1 wired `--show profile` only. This slice ships
`--show hooks`: re-walks each loaded profile's `hooks` block and
renders `event <- source-mixin` attribution so an operator can ask
"why isn't `on_revival` firing?" and immediately see whether (and
where) the hook is composed.

Self-filed follow-up.

## Bead(s)

- **bd-4b2a82** — primary (self-filed bd-5d2d83 follow-up).

## Before state

- `caco agent introspect --show profile` worked (bd-5d2d83 slice 1).
- `--show hooks` returned `not yet implemented` error.
- `load_composed_profile_for_agent_id` returned only the composed
  Profile + chain + dir, dropping the per-profile list. Source-mixin
  attribution was therefore impossible from the call site.

## After state

- Refactor: `load_composed_profile_for_agent_id` now returns a
  4-tuple `(composed, chain, profiles_dir, individual_profiles)`.
  Both existing callers updated (the auto-mute helper destructures
  with `_profiles`).
- New `derive_hooks_attribution(profiles)` helper: walks the same
  phase set / order as `compose_hooks` in caco-profile, returns
  `Vec<(event, Vec<source_mixin>)>` for events with at least one
  entry. Phases with no entries are omitted from output.
- `dispatch_agent_introspect`: now accepts `profile` (default) and
  `hooks`. Other `--show` values still error explicitly with the
  list of supported sections (helpful for operator typo).
- `--show hooks` text mode: padded "event <- source-mixin[, ...]"
  alignment so eyes parse quickly.
- `--show hooks` JSON: `{ agent_id, profile_chain, hooks: { event:
  [source-mixin, ...] } }` for jq filtering.
- Known gap (filed implicitly via bead description): doesn't honour
  `disable_hooks` yet, so introspect can over-report a hook the
  runtime would actually skip. Follow-up if it bites.

## Tests

- New: `derive_hooks_attribution_attributes_phases_to_source_mixins`
  — locks the per-event source-mixin attribution + ordering across
  3 fake profiles.
- New: `derive_hooks_attribution_empty_profiles_returns_empty`
  — locks the trivial path.
- All pre-existing introspect / load_composed_profile / derive_mute
  tests still pass.

## Validation

- `cargo test -p caco-cli --lib derive_hooks_attribution`: 2/2 PASS.
- `cargo test-small`: 4257+ tests across 8 binaries, 0 failures.
- `cargo clippy -p caco-cli -p caco-daemon -p caco-beads -p caco-web
  -p caco-profile --all-targets -- -D warnings`: clean.

## Diff summary

```
crates/caco-cli/src/lib.rs                       | ~+200 / -10
.cacophony/agent/.../summary/0019                | (new)
```

## Operator-takeaway

```bash
caco agent introspect --id <id>                # composed profile (slice 1)
caco agent introspect --id <id> --show hooks   # event <- source-mixin
caco agent introspect --id <id> --show hooks --json | \
  jq '.hooks | to_entries | map(select(.value | length > 1))'
  # find phases composed from multiple mixins (likely override hotspot)
```

If `--show hooks` shows a phase you expected but it's not firing, the
next likely culprit is a `disable_hooks: ["mixin:phase"]` entry in
one of the profile chain — diff that against the introspect output.
(introspect doesn't yet honour disable_hooks; future slice).

## Coordination

- Spoke claim with planned scope (self-filed bd-4b2a82 explaining
  hand-off from bd-5d2d83).
- Will speak completion + reintegrate.

## Notes for next time

- The `Default::default() + field assignment` clippy lint
  (`field-assignment outside of initializer for an instance created
  with Default::default()`) has bitten me twice now. Default to
  struct-update syntax (`..Default::default()`) on first write.
- `disable_hooks` honouring would close the symmetry between
  introspect output and runtime composition. Tag onto the next
  introspect-related bead if I take one.
