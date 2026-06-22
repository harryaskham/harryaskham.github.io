# Session summary — bd-babf1b: legible Pico transcript timestamps + sending label

## Goal

Fix a real WCAG legibility defect found by an evidence-driven visual pass of the
Pico pane: a vision-model description of a captured screenshot flagged the
conversation timestamps and the optimistic "sending" label as "very low-contrast
(dark text on dark bubbles, hard to read)". Make them readable without making
them prominent (they are subordinate metadata).

## Bead(s)

- `bd-babf1b` — caco-web Pico: low-contrast transcript timestamps and sending label (WCAG legibility)

## Before state

- Failing tests: none.
- `.pico-time` (timestamps) and `.pico-pending-label` ("sending" on optimistic
  outgoing bubbles) were both `color: var(--text-faint)` (#434c5e) — ~1.3:1 on
  the `rgba(59,66,82,0.62)` bubble fill, effectively unreadable. (The bold
  uppercase role labels survive `--text-faint`; normal-weight 11px text does not.)
- Vision pass: timestamps "very low-contrast … hard to read".

## After state

- Failing tests: none.
- `.pico-time` → `var(--text-dim)` (#5b6478), matching the established
  `.chat-time` timestamp convention. `.pico-pending-label` → `var(--text-muted)`
  (#7b88a1) so the active "sending" status is clearly legible.
- caco-web `--lib` 647 (incl. new guard); clippy clean.
- Vision re-check on the rebuilt pane: timestamp now "faint/low-contrast but
  readable … does not blend into the background" — legible yet still subordinate
  to the white body text, the intended result.

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `crates/caco-web/static/style.css` — `.pico-time` and `.pico-pending-label`
    colour tokens.
  - `crates/caco-web/src/tests.rs` — guard
    `pico_timestamps_and_pending_label_are_legible_bd_babf1b`.
- Tests: +1 regression guard.
- Behavioural delta: transcript timestamps and the "sending" label are now
  readable; no layout change.

## Embedded artefacts

- `web/contrast-observation.log` — Playwright pico-pane run after the fix.
- `web/screenshots/*.png` — rebuilt pane showing the legible timestamps.

## Operator-takeaway

`--text-faint` (#434c5e) is fine for bold uppercase labels but unreadable for
normal-weight metadata on the dark Pico bubbles. The dashboard already had the
right token for secondary timestamps (`--text-dim`, used by `.chat-time`); the
Pico pane just wasn't using it. Evidence-driven visual passes (describe a real
screenshot) catch this class of low-contrast metadata that unit tests never see.
