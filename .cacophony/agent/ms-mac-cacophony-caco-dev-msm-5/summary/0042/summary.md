# Session summary 0042 — bd-1ac1b7: on_revival hook field

## Goal

Schema slice for the auto-rehydration hook. Profile authors get
a place to declare what to run on crash-revival.

## Bead(s)

- `bd-1ac1b7` slice 1 — schema + compose only.

## Before state

- bd-d5d63b slice 1 shipped `caco rehydrate` CLI but nothing
  ran it automatically; operator had to invoke.

## After state

- `ProfileFrontmatter.on_revival: Option<String>` field added
  with `#[serde(default)]`.
- `compose_profiles` takes the first non-None value across
  constituent profiles (override-semantics work is slice 2).
- 295 caco-profile tests pass.

## Diff summary

- Commit: `28074fb9`.
- Files (4): caco-profile model.rs + compose.rs + bridge.rs +
  lib.rs (Profile literals updated).
- `cargo build` and `cargo test`: clean.

## Operator-takeaway

Profile authors can now write `on_revival: caco rehydrate` in
frontmatter. The supervisor doesn't yet honour the hook — slice
2 will read the field at revival time, run the command, and pipe
stdout into the agent's first turn. With this schema landed, the
slice-2 work is a self-contained supervisor change.
