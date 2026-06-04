# bd-86750c flicker slice — guard on-screen variants from byte-budget eviction

## Bead
bd-86750c (P1 bug, operator-directed): "Fix TUI graphics flicker and missing
chat/navigation backgrounds." Operator report: identical graphics at different
screen positions flicker in/out; chat bubbles + nav-subpanel backgrounds render
wrong. Multi-part umbrella; split across headless dev nodes (source) + msm-2
(live kitty validation).

## Slice landed (this reintegration)
The **upload-ID / virtual-placement flicker** slice. The chat-bubble slice
(bd-95ede3, po4-1) and nav-background slice land separately; the umbrella
bd-86750c stays OPEN pending those + msm-2 visual confirmation.

## Root cause
`crates/caco-tui/src/kitty.rs` `evict_global_retained_until_under_budget` (the
cross-surface retained-image byte-budget LRU eviction, bd-223eb5) popped
LRU-front `(surface_key, data_hash)` variants and reclaimed them purely by
age/byte pressure. It did **not** check whether the variant's kitty image ID was
still displayed on-screen this redraw. With many surfaces open (chat bubbles +
nav backgrounds), the byte budget could evict a **still-visible** retained
variant, forcing a terminal-side delete + re-upload on the next frame — the
operator-reported "identical graphics at different screen positions flicker
in/out." This is the same class as po4-1's chat-bubble find (a cleanup step
wiping a still-live placement), reached convergently.

## Fix
- New helper `currently_displayed_image_ids()`: the set of image IDs whose
  surface has `last_seen_redraw == redraw_count` and a live `displayed_image_id`.
- Eviction loop now **skips** any popped variant whose image ID is in that set,
  re-queuing it to the back of `global_retained_lru` so the budget reclaims
  genuinely off-screen variants first. The bounded `guard` still prevents
  pathological loops; if every remaining variant is on-screen, the budget is
  legitimately exceeded by live content and the loop stops rather than flicker
  visible placements.

## Tests
- New `global_byte_budget_skips_on_screen_variant_bd_86750c`: the on-screen
  LRU-front variant survives a tightened budget while the off-screen variant is
  reclaimed. Green.
- Existing `global_byte_budget_evicts_lru_across_surfaces` still green (no
  regression to off-screen eviction).
- Queued `cargo test -p caco-tui --lib global_byte_budget`: 2 passed, exit 0.
  Full `cacophony-fast-tests` gate (test-small + check --workspace --tests +
  clippy --workspace) runs at reintegration.

## Validation boundary
Source-validated headless on pocket4. **Live kitty visual confirmation routed to
msm-2** (only graphics-capable node): open many chat bubbles + nav panels and
watch for identical-bitmap flicker under retained-byte-budget pressure.

## Coordination
Split agreed with po4-1: po4-1 owns chat-bubble (bd-95ede3) + nav-background
(touching views/chat.rs, border_integration.rs, views/common.rs); po4-3 owns the
flicker logic in kitty.rs. Both feed msm-2. bd-95ede3 linked as a dependency of
the bd-86750c umbrella. Files stayed disjoint (I touched only kitty.rs).

## SPEC
SPEC 20.8 (TUI kitty graphics surface/placement lifecycle). No contract change;
this hardens the retained-byte eviction so it cannot de-draw a live placement.

## Diff
See the reintegration receipt for the landed squash SHA. Slice commit on the
agent branch: bd-86750c flicker fix in crates/caco-tui/src/kitty.rs
(currently_displayed_image_ids + eviction guard + unit test).
