# Session summary — Profile.composes_well_with metadata (bd-f30a29)

## Goal

Add a `composes_well_with` frontmatter field on profiles so authors can declare suggested mixin stacks ("dev + merge-queue + reflect-session") without committing them as a hard `composes:` resolution chain. Surfaces as documentation in profile-creator and any future `caco profile show`.

## Bead(s)

- `bd-f30a29` — Profile metadata: declare `composes_well_with: [list]` so profile-creator + operators see suggested mixin stacks at a glance

## Before state

- `Profile` struct had `composes: Option<Vec<String>>` for hard composition only. There was no way to advertise "you might want to layer X on top of me" without burying the guidance in prose.
- Two recently-authored mixins (`reflect-session.md`, `collab-mode.md`) carried the suggested stack in YAML comment / paragraph form only.

## After state

- `cargo test -p caco-profile --lib composes_well` — 3 / 3 passed.
- `cargo test-small` — 197 / 109 / 718 / 289 / 18 / 2805 / 51 passed, 0 failed.
- `cargo check --workspace --tests` — clean.

## Diff summary

- Commit: `e94441f4`
- Files touched:
  - `crates/caco-profile/src/model.rs` — adds `composes_well_with: Option<Vec<String>>` with `#[serde(default)]`.
  - `crates/caco-profile/src/{lib,compose,bridge}.rs` and `crates/caco-cli/src/lib.rs` — 7 `Profile` struct literals updated with `composes_well_with: None`. Mechanical.
  - `crates/caco-profile/src/lib.rs` — 3 new tests covering round-trip, no-validation, and default-None behaviour.
  - `.cacophony/profiles/reflect-session.md` and `collab-mode.md` — concrete operator-facing examples; each declares its suggested stack.
  - `.cacophony/profiles/profile-creator.md` — new "Documentation metadata" section under Frontmatter Reference so future profile-authoring agents discover the field.
- Tests: +3 unit; 0 removed; 0 flipped.
- Behavioural delta: zero. Resolution pipeline ignores the field.

## Operator-takeaway

Profile authors can now write:

```yaml
composes_well_with:
  - dev
  - merge-queue
  - reflect-session
```

and the field round-trips through the parser. Naming a non-existent profile is intentionally non-fatal so authors can suggest stacks without worrying about phantom-reference fragility. The field is *only* metadata — the resolver ignores it. The two mixin profiles I authored last week (`reflect-session`, `collab-mode`) are now annotated as live examples. A natural follow-up: `caco profile show <name>` to render the suggested stack as a section, and profile-creator-side validation that warns when `composes_well_with` references unknown profiles. Both deferred — this commit is the metadata field plus its documentation.
