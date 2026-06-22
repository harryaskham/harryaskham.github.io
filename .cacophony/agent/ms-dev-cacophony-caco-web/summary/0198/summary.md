# Session summary — bd-4cb15b final: generated PicoView wasm artifacts embedded

## Goal

Finish the caco-web shared PicoView reducer path by generating and embedding the actual wasm-bindgen artifacts consumed by the adapter, aligning the wasm-bindgen schema versions so the build is reproducible under the first-party `.#caco-web-wasm` shell.

## Bead(s)

- `bd-4cb15b` — caco-web pico pane: full RpcEvent reducer via shared wasm PicoView.

## Before state

- Prior partials had landed the adapter seam (`pico-view-adapter.js`), app.js shared-view path, cdylib crate type, and a `just pico-web-wasm` recipe/devshell.
- Running the recipe initially failed because the Rust crate used wasm-bindgen 0.2.118 while the Nix shell provided wasm-bindgen-cli 0.2.114. The artifacts were not yet generated or embedded.

## After state

- Workspace `wasm-bindgen` dependency is pinned to `=0.2.114`, matching the Nix shell CLI schema.
- Cargo.lock is updated to the matching wasm-bindgen/js-sys/web-sys family.
- `nix develop .#caco-web-wasm --command just pico-web-wasm` succeeds and writes:
  - `crates/caco-web/static/pico_view.js`
  - `crates/caco-web/static/pico_view_bg.wasm`
- caco-web source tests now pin both generated artifacts and verify `pico_view.js` exports `PicoView` methods (`apply_line`, `snapshot_json`, `images_json`, `prompt_line`).
- Validation is green: caco-web 635 tests, caco-picophony wasm-feature 79 tests.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched:
  - `Cargo.toml` / `Cargo.lock` — pin wasm-bindgen schema to the CLI version used by `.#caco-web-wasm`.
  - `crates/caco-web/static/pico_view.js` — generated wasm-bindgen JS glue.
  - `crates/caco-web/static/pico_view_bg.wasm` — generated shared PicoView wasm artifact.
  - `crates/caco-web/src/tests.rs` — bd-4cb15b artifact embedding/export guard.
  - `.cacophony/agent/.../summary/pending/web/validation.txt` — validation evidence.
- Tests: generated artifact checks added to the existing bd-4cb15b source guard.
- Behavioural delta: caco-web now loads the real shared `caco-picophony` PicoView reducer/helper module when serving static assets; the JS reducer remains a safety fallback if the module cannot load.

## Embedded artefacts

- `web/validation.txt` — command list, results, and implementation notes.

## Operator-takeaway

caco-web now has the same shared Picophony view core available in-browser that Android/iPhone consume via native bindings: generated `PicoView` wasm is embedded, loaded by the adapter, and used before the JS fallback. This closes the foundational reducer-parity bead and unlocks the next parity slices for composer commands, dialogs/model picker, images/widgets, and standalone embedding.
