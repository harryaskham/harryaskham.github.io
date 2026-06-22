# Session summary — caco-web active filter-chip contrast (bd-4217eb)

## Goal

Implement the targeted, clearly-fixable part of the contrast finding I filed as
bd-4217eb: the ACTIVE/selected `closed` and `stopped` filter chips rendered their
text + border in `--text-faint` (~2.2:1), failing WCAG even for UI components and
inconsistent with the other active filter chips (open→accent, in_progress/running
→success). The global `--text-faint` token (used on 31 mostly-decorative
elements) remains intentional de-emphasis and is left unchanged.

## Bead(s)

- `bd-4217eb` — caco-web contrast: --text-faint below WCAG on active filter chips.
  Filed as a draft this session from the contrast-audit probe; promoted + claimed
  + the targeted active-chip portion implemented here.

## Before state

- Failing tests: none (CSS-only).
- `.filter-chip.chip-status-closed.active` and `.filter-chip.chip-state-stopped.active`
  used `color: var(--text-faint)` + `border-color: var(--text-faint)` (#434c5e,
  ~2.2:1 on the dark bg) — a SELECTED/active control rendered below WCAG AA, and
  inconsistent with the readable accent/success colors the other active filter
  chips use.

## After state

- Failing tests: none.
- Both rules now use `--text-muted` (#7b88a1, ~5.2:1 — passes WCAG AA) for color +
  border, preserving the de-emphasized look (muted gray, distinct from the
  accent/success active states) while making the selected text readable.
- Validation: Playwright on the Beads view — activated the Closed filter, computed
  `color: rgb(123, 136, 161)` (= --text-muted) confirmed on the active chip (was
  rgb(67,76,94) = --text-faint). Console clean.

## Diff summary

- Code commit: final landed squash SHA from the reintegration receipt.
- Files touched: `crates/caco-web/static/style.css` (2 rules, +comments). No Rust.
- Behavioural delta: active closed/stopped filter chips are now AA-readable;
  global `--text-faint` token + its decorative usages unchanged (intentional
  de-emphasis); other active chips unchanged.

## Embedded artefacts

- `web/screenshots/after-active-closed-readable.png` — Beads view with the Closed
  filter active, chip text in the readable muted color.

## Operator-takeaway

The contrast audit (bd-4217eb draft) split cleanly into a clear bug (active
controls failing WCAG) and a design decision (the global de-emphasis token). I
implemented the clear part and left the token as intentional de-emphasis — a
selected filter should always be readable, but the faint decorative token across
31 separators/dots/comments is a deliberate hierarchy choice, not a defect.
Landed via the controller-authorized CSS-only --skip-hooks fast path.
