# Session summary — Generalize shared-owner multi-area graphics retire fix (bd-8a1fd0)

## Goal

Operator directive from Harry: after the chat-bubble (bd-95ede3) and
nav-background (bd-86750c) graphics fixes landed, "stamp this type of problem out
everywhere it occurs." Rather than keep patching individual views, generalize the
shared-owner / multi-area-per-frame retire-wipe fix so it protects EVERY graphics
owner automatically.

## Bead(s)

- `bd-8a1fd0` — Generalize shared-owner multi-area retire fix to all graphics
  owners (stamp out the class) (P2 bug, caco-tui)
- generalizes: `bd-12879e` (sidebar) + `bd-95ede3` (chat); covers the
  `bd-8eaa06` agent-detail/chat missing-borders report; related umbrella
  `bd-86750c`.

## Before state

- Failing tests: none.
- The shared-owner retire-wipe bug class was fixed pointwise only for
  `sidebar:navigation` (bd-12879e) and `chat:*` (bd-95ede3) via hardcoded
  owner-prefix checks in flush_graphics_requests.
- Still-affected owners (verified by scanning with_graphics_owner +
  PanelRole::Subpanel co-registration): agent-detail:{id}, actions,
  configuration, console:{id}, crons, bead_detail, builds:{project},
  beads:{project}, summaries:{project}, persistent-detail:{id},
  agent_detail:inner_tabs:{id}. Each co-renders an outer panel + ≥1 subpanel
  (or N subpanels) under one owner at different rects, so the owner-area-move
  retire (bd-387b9d) wiped all-but-the-last sibling every frame.

## After state

- Failing tests: none. caco-tui lib: 4149 passed / 0 failed; clippy clean
  (-D warnings); rustfmt clean.
- flush_graphics_requests now runs a per-frame pre-pass computing
  multi_area_shared_owners (owners that register >1 distinct area this frame).
  The rect-move-retire skip is driven by that set, subsuming the sidebar/chat
  prefix checks. General shared owners collect their border keys into a new
  shared_owner_border_surfaces active set, retired via replace_graphics_surface_set
  (correct per-sibling add/keep/retire, no mid-frame wipe). No allowlist to maintain.
- Per-surface visual confirmation across views (agent-detail, actions,
  configuration, etc. all show borders) needs an operator at a kitty terminal —
  folds into the same single visual pass already pending for bd-95ede3/bd-86750c.

## Diff summary

- Code commit: 2fbb38937 (final landed squash SHA from reintegration receipt).
- Files touched: `crates/caco-tui/src/app.rs` (one file).
- Changes: per-frame multi_area_shared_owners pre-pass; generalized
  is_shared_multi_area_owner detection; new shared_owner_border_surfaces field +
  active-set collection + replace_graphics_surface_set retire; sidebar/chat
  behavior preserved (routed to their existing sets via && !is_sidebar && !is_chat).
- Tests: +2 (general_shared_owner_multi_area_does_not_retire_earlier_sibling_bd_8a1fd0
  behavioral; flush_graphics_requests_detects_multi_area_shared_owners_generally_bd_8a1fd0
  source invariant); updated the bd-95ede3 invariant to the general check.
- Behavioural delta: every multi-panel graphics owner retains all its borders/
  backgrounds across a frame instead of dropping all-but-the-last to text.

## Embedded artefacts

None (headless node; visual confirmation deferred to graphics-capable host).

## Operator-takeaway

The chat-bubble and nav-background bugs were two instances of one class: a shared
graphics owner drawing multiple boxes at different rects in one frame, where a
per-request cleanup wiped each earlier box as the next drew. Instead of chasing
each view, this makes flush_graphics_requests detect the pattern itself — any owner
with >1 area this frame is protected automatically — so agent-detail, actions,
configuration, console, crons, bead-detail, builds and every future multi-panel
owner are all covered with no list to keep updated. This is the "stamp it out
everywhere" fix Harry asked for. Final visual sign-off across views still needs the
operator at a real kitty terminal.
