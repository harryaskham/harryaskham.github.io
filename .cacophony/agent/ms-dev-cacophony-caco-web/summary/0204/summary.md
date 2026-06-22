# Session summary — bd-e21549: Picophony docs updated for caco-web parity

## Goal

Update the Picophony parity documentation so it reflects the now-shipped caco-web native Pico surface rather than describing the browser as a missing gap.

## Bead(s)

- `bd-e21549` — [pico] caco-web: update Picophony parity docs after browser surface catches up.

## Before state

- `docs/design/picophony-surface-parity.md` still said caco-web was missing and pointed to `bd-2d7203` as future work.
- `docs/design/picophony.md` described the generic wasm binding but did not mention the concrete caco-web consumption path, reusable component, `/pico`, or observe proof.

## After state

- caco-web is marked `Native present` in the surface matrix.
- The docs list the shipped caco-web parity slices: baseline, styling/a11y, shared wasm reducer, shared composer, dialogs/model picker, images/widgets, embeddable/standalone route, and deterministic observe evidence.
- Added caco-web browser surface notes explaining `window.CacoPicoAgentView.mount`, `/pico?agent=<id>`, `pico-view-adapter.js`, generated `pico_view.js`/`.wasm`, and `caco-web-observe --scenario pico-pane`.
- Validation confirmed stale “missing caco-web” wording is gone and source anchors still pass.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched:
  - `docs/design/picophony-surface-parity.md`
  - `docs/design/picophony.md`
- Tests/checks: grep checks for stale/new doc anchors; focused caco-web test for the embeddable/standalone anchor.
- Behavioural delta: documentation only.

## Embedded artefacts

- `web/validation.txt` — validation commands/results.

## Operator-takeaway

The browser is no longer the missing Picophony surface in the docs: caco-web is documented as native-present with its reusable component, standalone route, shared wasm core, and deterministic observe proof.
