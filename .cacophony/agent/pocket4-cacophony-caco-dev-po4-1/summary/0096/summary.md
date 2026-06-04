# Session summary — Chat bubbles render text-mode (bd-95ede3)

## Goal

Operator reported chat message bubbles render in plain text mode (no kitty
graphics border) while every other TUI panel renders with kitty graphics, and
prior fixes hadn't stuck. The goal was to find the real root cause and fix it so
chat bubbles get their kitty graphics borders like the rest of the UI. This is a
slice of umbrella bd-86750c (Harry's multi-part graphics report).

## Bead(s)

- `bd-95ede3` — Chat bubbles render in text mode instead of kitty graphics
  despite config (P2 bug, caco-tui; child of umbrella bd-86750c)
- parent: `bd-86750c` — Fix TUI graphics flicker and missing chat/navigation
  backgrounds (P1, owned by po4-3; closes once this + nav slices land + msm-2
  visual confirm)

## Before state

- Failing tests: none (visual bug; the recording path was correct in isolation).
- Symptom: chat bubbles text-mode; all other roles kitty. Prior fixes ineffective
  because they targeted the (correct) record path, not the downstream retire.
- Root cause: chat surfaces bind the message-list Panel chrome + every per-message
  Bubble panel to ONE shared `chat:*` scene owner, but each bubble has a distinct
  rect. The non-sidebar branch of flush_graphics_requests calls
  mark_ratatui_surface_area_live(owner, area) per request (bd-387b9d owner-area-move
  retire), so each later bubble's rect was treated as a MOVE and wiped the earlier
  bubbles' surfaces mid-frame — all but the last bubble dropped to text every frame.

## After state

- Failing tests: none. caco-tui lib: 4146 passed / 0 failed; clippy clean; rustfmt clean.
- chat:* scene owners now get the same shared-owner / multi-area treatment the
  sidebar already had (bd-12879e): skip the per-request rect-move retire and drive
  per-bubble add/keep/retire through a per-frame active set
  (replace_graphics_surface_set), so earlier bubbles are no longer wiped mid-frame.
- Visual confirmation pending on graphics-capable host (msm-2) — both pocket4 and
  msm-2 are headless managed agents; the fix is validated by behavioral +
  source-invariant unit tests headless, with live visual confirm to follow.

## Diff summary

- Code commit: 18ae7a68e (final landed squash SHA from reintegration receipt).
- Files touched: `crates/caco-tui/src/app.rs` (one file).
- Changes: generalized is_sidebar_shared_owner into is_sidebar/is_chat shared-owner
  detection (chat:* prefix); added chat_border_surfaces field + per-frame active-set
  collection + replace_graphics_surface_set retire, mirroring the sidebar bd-12879e
  pattern.
- Tests: +2 (chat_bubbles_share_owner_without_retiring_earlier_bubbles_bd_95ede3
  behavioral; flush_graphics_requests_treats_chat_owner_as_shared_owner_bd_95ede3
  source invariant).
- Behavioural delta: chat bubbles retain their kitty graphics borders across a
  multi-bubble frame instead of falling back to text.

## Embedded artefacts

None (headless node; visual confirmation deferred to graphics-capable host).

## Operator-takeaway

The chat-bubble "text mode" bug was NOT in the bubble recording path (which is
why earlier fixes didn't stick) — it was a shared-owner/multi-area cleanup bug:
bubbles share one scene owner but draw at different rects, and the per-request
owner-area-move retire wiped each bubble as the next one drew. The sidebar had the
exact same bug and was already fixed (bd-12879e); this applies the same pattern to
chat. po4-3's sibling flicker slice (bd-86750c, kitty LRU evicting on-screen
placements) is the same root-cause CLASS — cleanup wiping still-live placements —
so the umbrella is a coherent family. Live visual confirm by a graphics-capable
node is the last step before the umbrella closes.
