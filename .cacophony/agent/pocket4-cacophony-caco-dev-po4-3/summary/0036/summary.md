# bd-86750c follow-up — byte-budget eviction on-screen-skip diagnostic (msm-2 review)

## Bead
bd-86750c (P1 umbrella, operator-directed): "Fix TUI graphics flicker and
missing chat/navigation backgrounds." This is a follow-up slice on top of the
flicker fix already landed at `6a6034f2e`.

## Context
The flicker root cause + fix (skip currently-displayed image IDs in the global
byte-budget eviction loop so it cannot de-draw a visible placement) landed at
`6a6034f2e` with `currently_displayed_image_ids()` + the eviction guard + the
`global_byte_budget_skips_on_screen_variant_bd_86750c` unit test.

msm-2 then did a rigorous static graphics-lane review and confirmed both the
root cause (eviction was unconditional, no on-screen check) and the fix shape.
Two review checks:

1. **Predicate must read the lifecycle/placement map, not a `RetainedImage`
   field.** CONFIRMED already satisfied: `currently_displayed_image_ids()`
   iterates `self.surfaces` (the `ReservedSurface` lifecycle/placement map),
   keeping image IDs where `last_seen_redraw == redraw_count` and
   `displayed_image_id` is set; eviction matches the evicted variant's
   `image_id` against that set.
2. **Loop must terminate when over budget but all remaining variants are
   on-screen, ideally with a diagnostic instead of silently exiting.** The
   `guard = global_retained_lru.len()` with `guard -= 1` before the skip check
   already bounds it (each entry examined at most once, no spin). This slice
   adds the requested observability.

## Change (this reintegration)
- New `byte_budget_skipped_on_screen: u64` diagnostic counter (+ accessor),
  initialized in both `SurfaceManager` constructors, incremented each time the
  byte-budget eviction loop skips a still-displayed variant.
- A persistently climbing value while `retained_total_bytes` stays over budget
  signals live on-screen content legitimately exceeds the cap (cap tuned too
  low for the active surface set) — distinct from a leak or misbehaving
  eviction.
- Extended `global_byte_budget_skips_on_screen_variant_bd_86750c` to assert the
  counter records the skip.

## Tests
- Queued `cargo test -p caco-tui --lib global_byte_budget`: 2 passed, exit 0
  (`global_byte_budget_skips_on_screen_variant_bd_86750c` +
  `global_byte_budget_evicts_lru_across_surfaces`, no regression).
- Full `cacophony-fast-tests` gate runs at reintegration.

## Validation boundary
Source/logic-validated headless and code-reviewed by msm-2 (both points green).
Live kitty visual flicker repro still needs a human-attended terminal (all
managed graphics agents are headless) — routed to the operator.

## SPEC
SPEC 20.8 (TUI kitty graphics surface/placement lifecycle). No contract change;
adds a diagnostic counter on the existing retained-byte eviction path.

## Diff
See the reintegration receipt for the landed squash SHA. Follow-up to the
`6a6034f2e` flicker fix; umbrella bd-86750c remains open pending po4-1's
chat-bubble slice (bd-95ede3), the nav-background slice, and an operator-side
live visual confirmation of the full report.
