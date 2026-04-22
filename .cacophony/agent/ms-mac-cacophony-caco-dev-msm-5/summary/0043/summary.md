# Session summary 0043 — bd-2188ce: self_nudge_interval_secs field

## Goal

Schema slice for the periodic self-nudge cadence so persistent
agents whose natural idle exceeds the restart-watchdog window
don't die just from being idle.

## Bead(s)

- `bd-2188ce` slice 1 — schema + compose only.

## Before state

- caco-ctrl naturally idles 30+ min between bead-filing cycles.
- 1800s restart watchdog kills it before the next cycle.
- No declarative way to say "I'm slow but alive — nudge me."

## After state

- `ProfileFrontmatter.self_nudge_interval_secs: Option<u64>`
  field with `#[serde(default)]`.
- `compose_profiles` takes `min` of the non-None values from
  constituent profiles (most-frequent-wins; safe because
  nudging more often than required is harmless).
- 295 caco-profile tests pass.

## Diff summary

- Commit: `5ee5787a`.
- Files (4): caco-profile model.rs + compose.rs (+ test fixture)
  + bridge.rs + lib.rs (Profile literals updated).
- `cargo build` and `cargo test`: clean.

## Operator-takeaway

Profile authors can declare `self_nudge_interval_secs: 300` in
frontmatter today. The supervisor doesn't yet honour the field
— slice 2 reads it at agent-start and registers a periodic wake
that fires a no-op nudge to keep the watchdog satisfied.
Composition uses smallest-interval-wins, so a stricter mixin can
tighten cadence without weakening anything.
