# Session summary — TUI Quickfile image upload (image→beads)

## Goal

Add image upload to the native TUI Quickfile surface so an operator can turn a
screenshot or image into beads without leaving the terminal. The slice wires a
new image-path input into the existing Quickfile flow and submits it to the
already-landed surface-agnostic `beads/expand` image→beads backend, rendering
the proposed beads through the normal Quickfile review affordances.

## Bead(s)

- `bd-0545e3` — TUI Quickfile image upload (image→beads)
- parent: `bd-7e883b` — Quickfile image→beads vision feature (multi-surface)
- depends on (landed): `bd-f04b54` — surface-agnostic image→beads backend pipeline

## Before state

- Failing tests: none (clean main).
- The TUI Quickfile dialog (`b`) only accepted free text; the `beads/expand`
  backend already accepted `image_base64`/`image_media_type` (bd-f04b54) and the
  CLI exposed `caco bd expand --image`, but the TUI had no image entry point.
- `QuickFileBeadField` had Project/Text/Results; `expand_beads()` took text only.

## After state

- Failing tests: none. 30 `quick_file` lib tests pass (3 new), `cargo check
  -p caco-tui --tests` clean.
- TUI Quickfile dialog now has an Image input panel between the project picker
  and text area. Operator types/pastes a path, presses Enter (or Ctrl+Y) to
  submit. Image is read + base64-encoded up-front; image-only submit is allowed;
  a bad path toasts an error without flipping `submitting`. Created beads render
  through the existing review/accept list. Image path clears after a successful
  expand so a later text-only resubmit/refine does not silently reattach it.

## Diff summary

- Code/content commit: b6dff474a (pending final squash SHA from reintegration receipt).
- Summary artefact commit: intentionally omitted (must not self-reference).
- Files touched: crates/caco-tui/src/state/mod.rs (QuickFileBeadField::Image,
  image_path TextBuffer, focus cycle), crates/caco-tui/src/client.rs
  (expand_beads optional image args), crates/caco-tui/src/app.rs (submit
  read/encode + guards, Image key/paste handling, read_quick_file_image +
  quick_file_image_media_type helpers, Image render panel, success-path clear,
  tests).
- Tests: +3 (image media-type detection, image-only submit flips submitting,
  missing-path keeps dialog open); updated focus-cycle test for the new Image
  field. flipped 0 / removed 0.
- Behavioural delta: TUI Quickfile gains image→beads input; Web/Android remain
  out of scope per the slice decomposition.

## Operator-takeaway

The TUI half of the image→beads vision feature is done and reuses the existing
expand pipeline end-to-end (CLI `--image`, backend vision describe, TUI panel),
so the only remaining parent-epic surfaces are Web and Android. Image bytes are
read on the UI thread at submit time; that is fine for screenshots but a very
large image could briefly block render — a future tightening could move the read
into the spawned task if it ever matters.
