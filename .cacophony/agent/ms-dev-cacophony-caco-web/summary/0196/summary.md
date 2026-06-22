# Session summary — bd-4cb15b partial: caco-web PicoView wasm adapter seam

## Goal

Continue caco-web pico parity after the visible styling slice by adding the first shared-core seam for `caco-picophony` wasm `PicoView`. The goal of this partial was to make caco-web capable of using the shared reducer/helper module when generated wasm artifacts are present, while preserving the current JS fallback so production remains safe.

## Bead(s)

- `bd-4cb15b` — caco-web pico pane: full RpcEvent reducer via shared wasm PicoView. This bead remains **in progress** after this partial; the generated artifact/reducer-replacement work is not complete yet.

## Before state

- caco-web had a slice-1 JS subset reducer in `app.js` (`applyPicoEvent`) and no wasm adapter/static loading seam.
- `caco-picophony` had a `wasm` feature and `PicoView` binding but its crate type did not include `cdylib`, so standard wasm-bindgen browser artifact generation was not represented in the crate manifest.
- Local node lacks `wasm32-unknown-unknown` and `wasm-bindgen` CLI, so full artifact generation cannot be validated on this host yet.

## After state

- `caco-picophony` crate type includes `cdylib` for wasm-bindgen output.
- caco-web embeds `pico-view-adapter.js`, a failure-tolerant dynamic adapter expecting generated `/pico_view.js` and `/pico_view_bg.wasm` when available.
- `index.html` loads the adapter before `app.js`.
- `app.js` attempts to create a shared `PicoView` before opening `/session`, applies raw incoming WebSocket lines through `view.apply_line`, renders `snapshot_json`, and falls back to the existing JS reducer if wasm is absent or fails.
- `picoPromptEnvelope` uses `PicoView.prompt_line` when the shared module is loaded.
- Validation is green: caco-web 634 tests, caco-picophony wasm-feature 77 tests.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched:
  - `crates/caco-picophony/Cargo.toml` — add `cdylib` crate type.
  - `crates/caco-web/static/pico-view-adapter.js` — new optional wasm adapter.
  - `crates/caco-web/static/index.html` — load adapter before app.js.
  - `crates/caco-web/static/app.js` — optional shared `PicoView` state/reducer seam and prompt helper use.
  - `crates/caco-web/src/tests.rs` — source guard for adapter, app seam, and cdylib manifest contract.
- Tests: +1 caco-web source test; caco-picophony wasm-feature tests also run.
- Behavioural delta: no visible change when wasm artifacts are absent; when generated PicoView artifacts are present, caco-web can use the shared view reducer/helper path before falling back to JS.

## Embedded artefacts

- `web/validation.txt` — commands, results, implemented partial, and remaining work.

## Operator-takeaway

This is the foundational shared-core seam, not the end of `bd-4cb15b`: caco-web is now wired to consume `PicoView` wasm safely, but the actual generated artifacts and full reducer replacement still need the next slice in a wasm-capable build path.
