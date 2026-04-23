# Session summary — Bead-close confirm visibility under modal (bd-b6a93d)

## Goal

Restore visibility of the bead-close confirmation prompt when
launched from inside the bead-detail modal. Operator could not
see the prompt (it rendered behind the bead-detail modal),
forcing a guess-or-Escape interaction.

## Bead(s)

- `bd-b6a93d` — Fix bead close confirmation visibility under
  modal (P2, modal, ui, webapp, z-index)

## Before state

- `crates/caco-web/static/style.css` had two competing rules:
  - `.modal-overlay { z-index: 1000 }` (line ~2427) — used
    by `bead-detail-modal` and other dialogs.
  - `.confirm-overlay { z-index: 300 }` (line ~4254) — used
    by the `showConfirm()` helper in `app.js` for every
    confirmation prompt.
- Because confirm-overlay has 300 < 1000, ANY confirm
  triggered while a modal-overlay was open (e.g. "Close bead?"
  launched from inside `bead-detail-modal`) rendered BENEATH
  the modal. Operator saw the dim backdrop change but no
  prompt buttons; could only escape via Esc-key by guessing.

## After state

- `.confirm-overlay { z-index: 1100 }` — sits cleanly above
  `.modal-overlay` (1000), the cluster-pulse overlay (any
  z-index ≤ 1000), and below the doctor-toast tier (2000)
  and overlay-blocker (3376 = 2000-class).
- Comment block at the rule explains the bug and the layering
  contract so future edits don't regress.
- New regression test
  `tests::confirm_overlay_layers_above_modal_overlay` parses
  the actual `style.css` z-index values out of both rules and
  asserts `confirm_z > modal_z`. Fails if either rule is
  missing or layered incorrectly.

## Diff summary

- Files touched:
  - `crates/caco-web/static/style.css` — bumped
    `.confirm-overlay` z-index 300 → 1100 with explanatory
    comment
  - `crates/caco-web/src/tests.rs` — added regression test
    that pulls z-index values out of the actual CSS and
    asserts the layering contract
- Tests: +1 / -0 / flipped 0
- Test command:
  `cargo test -p caco-web confirm_overlay_layers_above_modal_overlay`
  → 1 passed in 0.02s after 22.6s compile.

## Operator-takeaway

Surgical CSS fix. The regression test validates the layering
contract by parsing the actual CSS rather than asserting
hardcoded values (so the next operator who needs to bump the
confirm tier can do so freely as long as it stays above
modal-overlay).

Honored constraints:
- Did not touch `caco-daemon` / `caco-sidecar` tests
  (caco-tui agent reported a separate compile break in
  `TopLevelBeadsConfig::peer_consult_timeout_ms` initialisers
  — out of scope for this bead; spoke to confirm I won't step
  on their patch).
- No `cargo test --workspace`; targeted single-test run only
  per bead-burndown protocol.
- Operator note "narrator stopped" honored — claim, in-flight
  acknowledgement of caco-tui's compile note, and close speak
  all issued by msm-2 directly.

This is the 12th bead I've closed this session counting
across both turns: bd-43db78, bd-aac755, bd-33ca18, bd-83a8ed,
bd-b9e32e, bd-84d22a, bd-75989e, bd-6ff0a0, bd-f6d1ea,
bd-fca3e1 (turn 1) + bd-1975be, bd-db60a1, bd-f32dda,
bd-2ba632, bd-b6a93d (this turn). Plus 4 reflections + 9
summaries landed.
