# Session summary — bd-327f8f one-image-per-type collision: harden shared-image deletes

## Goal

Investigate the P1 graphics bug where only one subpanel background renders and
borders go missing on sibling panels ("one-image-per-type collision"), determine
whether the shared-image dedup path collapses per-surface placements, and land a
durable safeguard. The deliverable is evidence about the real cause plus a
regression guard, since the visual symptom requires a real terminal that this
headless node cannot drive.

## Bead(s)

- `bd-327f8f` — TUI graphics: one-image-per-type collision — shared-image dedup
  may collapse per-surface placements (passthrough)
- Related context: `bd-664462` / `bd-4fc613` (fast_text_paint_defer gate),
  `bd-e5b280` / `bd-5f4eb5` / `bd-db6142` (shared retained-image accounting).

## Before state

- Failing tests: none (bug is a visual rendering symptom, not a red test).
- The bead hypothesised a Unicode-placeholder / passthrough encoding collapse on
  shared image_ids, with `fast_text_paint_defer` named as a confounder.
- No regression test covered two byte-identical sibling surfaces (shared deduped
  image_id) keeping distinct placements across frames and through retire.

## After state

- Failing tests: none. Full `caco-tui` `kitty::` test module passes (exit 0),
  `cargo clippy -p caco-tui --lib` clean (exit 0).
- Findings established and cross-confirmed with aur-3 (independent trace):
  1. This TUI does NOT use Unicode-placeholder virtual placements. It uses
     cursor-positioned classic Kitty placements (`a=p` / `a=T` with `C=1`). The
     bead's Unicode-placeholder hypothesis does not match the code.
  2. Shared dedup keys distinct `placement_id` (= `surface_id`) under a shared
     `image_id`; `active_image_placements` ref-counts them;
     `release_displayed_surface_image` issues a per-placement `d=i,p=` delete,
     not an image-level `d=I`, while a sibling is live.
  3. `fast_text_paint_defer` defaults to `false`, so its `invalidate_all`
     completion path is not armed in default config and cannot be the
     default-config cause.
- Added a defensive guard `other_surface_displays_image` so any image-level
  `d=I` delete is downgraded to a placement-only delete when another live
  surface still displays the deduped shared image — preventing the collapse even
  if the active-placement ref-count ever drifts.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `crates/caco-tui/src/kitty.rs`.
- Tests: +2 (`identical_sibling_surfaces_keep_distinct_placements_bd_327f8f`,
  `release_shared_image_with_drifted_refcount_spares_sibling_bd_327f8f`); plus a
  reusable `run_batch_upload_pass` test helper that mirrors the real per-frame
  app.rs batch upload ordering.
- Behavioural delta: image-level shared-image deletes are now guarded against
  collapsing sibling placements. No change to the common case (still emits a
  placement delete when ref-count says a sibling is active); the guard only
  intercepts the otherwise-collapsing `d=I` path when a sibling still displays
  the image.

## Operator-takeaway

The shared-image SurfaceManager logic is correct and now regression-guarded: two
independent traces (mine and aur-3's) plus a passing per-frame reproduction test
show identical sibling panels keep distinct placements through frames and
retire. The visible one-image-per-type symptom, if it still reproduces in
default config, is most likely a terminal-level passthrough rendering behavior
rather than a SurfaceManager bug — it needs a real Kitty/tmux terminal capture to
confirm, which this headless aurora node cannot provide. The landed defensive
guard makes the collapse impossible from the daemon side even under ref-count
drift; the remaining verification should be owned by a node with a real terminal
(caco-macos / a TUI-capable host). Recommend re-checking the live symptom on a
fresh rebuild before any deeper surface-manager rework.
