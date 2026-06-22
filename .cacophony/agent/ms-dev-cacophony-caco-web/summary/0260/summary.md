# Session summary — bd-da0eea: legible .pico-role speaker label (bd-babf1b missed sibling)

## Goal

Fix the per-bubble speaker label (.pico-role: YOU / assistant / thinking / note /
tool) which was still the too-faint var(--text-faint) (~1.3:1 on the bubble fill)
— the sibling that bd-babf1b missed when it fixed .pico-time and
.pico-pending-label. A vision pass over the bd-87a45b render independently
flagged the "YOU" label as low-contrast.

## Bead(s)

- `bd-da0eea` — .pico-role speaker label still uses too-faint --text-faint
- Sibling of `bd-babf1b` (timestamps + pending label legibility).

## Before state

- `.pico-role { color: var(--text-faint); }` (#434c5e), ~1.3:1 against the bubble
  fill — the speaker label on every bubble was hard to read.

## After state

- `.pico-role { color: var(--text-muted); }` (#7b88a1), ~3.2:1 (bold 11px
  uppercase) — clearly legible, matching the adjacent .pico-pending-label.
- caco-web `--lib` 651 (the bd-babf1b legibility guard extended to pin .pico-role);
  clippy clean.

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `crates/caco-web/static/style.css` — .pico-role color --text-faint -> --text-muted.
  - `crates/caco-web/src/tests.rs` — extended the legibility guard to pin .pico-role.
- Tests: +1 assertion in the existing legibility guard.
- Behavioural delta: every Pico bubble's speaker label is now readable.

## Embedded artefacts

- None.

## Operator-takeaway

The --text-faint token (#434c5e) is effectively unreadable on the bubble fill;
bd-babf1b fixed two of its three Pico uses and this completes the set. Worth a
broader sweep: grep for any remaining var(--text-faint) on text that must be read.
