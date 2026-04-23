# Session summary — Quick-file bead preview-before-create (bd-150104)

## Goal

Add a preview-confirm step to the webapp's quick-file bead
"File as-is" path so operators can review the parsed
title/description/type/priority/project BEFORE the POST fires.
Closes the foot-gun where any typo in the textarea's first
line became a permanent bead title until manually closed.

## Bead(s)

- `bd-150104` — Add preview before create for quick file bead
  dialog (P2, UI, UX, dialogs, file-creation)

## Before state

- Two paths from the quick-file modal:
  - **AI Expand path** — already showed structured preview
    cards (the daemon's `/beads/expand` returns parsed beads
    that the operator reviews before nothing — ALREADY
    persisted by the daemon per bd-1ffeb0).
  - **File as-is path** — `quickBeadDirect()` fired the POST
    immediately on click. No preview, no confirm. Typo in the
    first line = permanent bead title until manual close.

## After state

- `quickBeadDirect()` now stages the parsed bead into
  `state._pendingDirectBead` and renders a preview card in
  the existing `quick-bead-preview` slot with explicit
  Cancel + Create buttons.
- `confirmQuickBeadDirect()` reads the staged bead and
  delegates to `quickBeadDirectFire()` for the POST.
- `cancelQuickBeadPreview()` clears the staged bead +
  preview slot but leaves the operator's textarea text
  intact so they can edit and re-preview without retyping.
- `quickBeadDirectFire()` is the factored POST + integrate
  flow (one implementation shared between the
  preview-confirm path and the defensive fallback path
  when the preview slot is missing).
- `showQuickBeadModal()` now clears
  `state._pendingDirectBead = null` on every modal open so
  a prior open-then-cancel cycle can't leak a stale staged
  bead into a fresh modal session.
- AI Expand path unchanged (already had the preview).

## Diff summary

- Files touched:
  - `crates/caco-web/static/app.js` — `quickBeadDirect`
    refactor + 3 new functions (`confirmQuickBeadDirect`,
    `cancelQuickBeadPreview`, `quickBeadDirectFire`) + state
    reset in `showQuickBeadModal`
  - `crates/caco-web/src/tests.rs` — new regression test
    `app_js_quick_file_bead_previews_before_create`
- Tests: +1 / -0 / flipped 0
  - Source-level contract test asserts presence of all four
    surface-area names (`confirmQuickBeadDirect`,
    `cancelQuickBeadPreview`, `quickBeadDirectFire`,
    `_pendingDirectBead`) AND scopes the
    `_pendingDirectBead = null` reset assertion to the
    `showQuickBeadModal` function body so a future refactor
    that moves the reset elsewhere lights up.
- Test command:
  `cargo test -p caco-web app_js_quick_file_bead_previews_before_create`
  → 1 passed in 0.03s after 12.9s recompile.

## Operator-takeaway

Quick-file flow now:

1. `b` keyboard shortcut → modal opens.
2. Type text. First line = title; rest = description.
3. Either:
   - "Expand with AI" → AI structures + creates beads (no
     change).
   - **"File as-is" → preview card appears (no POST yet).**
   - **Operator clicks Create → bead is filed.**
   - **OR operator clicks Cancel → text preserved, modal stays
     open for editing.**
4. ESC / overlay-click → modal closes. State cleared.

The preview surfaces:
- Priority badge (P2 default)
- Type tag (`task` default)
- Parsed title + description
- Project name (so cross-project misfires get caught)

If you want richer preview (label parsing from `#tag`
syntax, type/priority overrides, etc.) that's a follow-up
bead — this lands the minimum viable preview that closes the
typo footgun.

Honored constraints:
- No `cargo test --workspace`; targeted single-test run.
- No daemon changes — pure webapp UX fix.
- Operator no-narrator rule honored — claim + close speaks
  issued by msm-2 directly.

18th bead closed this session (cumulative). 11th in this turn.
