# bd-34b6cc — caco-tui: consume daemon AttachMetadata.pico_socket for pico attach

## Bead
bd-34b6cc (caco-tui/picophony, P2) — self-filed follow-up to bd-b95687 (pico attach wiring), consuming aurora's M6 increment (bd-019da7). Aurora landed `pico_socket: Option<String>` on the daemon AttachMetadata (`686635cce6`) and pinged me to swap the hardcoded socket derivation for the metadata.

## Change
The daemon's `/api/v1/agents/<id>/attach` response now carries `pico_socket: Option<String>` — `Some(<agent_dir>/pico.sock)` for LOCAL `agent_type=="pico"` agents, `None` for non-pico or remote (remote routes via the `/session` WS bridge, aurora's M7). This makes the TUI consume that authoritative path instead of always deriving it locally.

### crates/caco-tui/src/state/mod.rs
- `AttachMetadata.pico_socket: Option<String>` (Debug/Clone struct; not Deserialize).

### crates/caco-tui/src/client.rs
- `AttachApiResponse.pico_socket: Option<String>` (`#[serde(default)]`).
- `fetch_attach_metadata` maps it into the built `AttachMetadata`.

### crates/caco-tui/src/app.rs
- New pure helper `resolve_pico_socket_path(metadata_socket, checkout_path) -> Option<String>`: the daemon-provided socket wins when present+non-empty; otherwise derive `<agent_dir>/pico.sock` from the checkout parent; `None` when neither.
- `pico_socket_for` (bd-b95687) now delegates to it, keeping the `runtime_dir()/agents/<project>/<id>/pico.sock` last-resort fallback for the rare case where neither is available.
- 17 other `AttachMetadata { … }` literals (real synthesis path + shell-tile/test helpers) updated with `pico_socket: None` (struct has no Default, so every literal must set the field).

## Tests
`pico_socket_prefers_metadata_over_derivation_bd_34b6cc`: metadata wins over the checkout derivation; absent metadata derives `<agent_dir>/pico.sock`; empty metadata is ignored (falls back to derivation); neither yields `None`.

## Behavior
For LOCAL pico agents the metadata path and the old derivation are identical (`<agent_dir>/pico.sock`), so this is authoritative-source belt-and-suspenders today; it sets up the contract so when remote pico attach lands (aurora's M7 `/session` bridge), the TUI already prefers daemon-supplied paths. Fallback preserves correctness against older daemons that don't send the field.

## Validation (daemon test queue, --cwd at checkout)
- `cargo test -p caco-tui --lib bd_34b6cc` (tj-468b5067): PASSED 1/1 (lib+tests compile).
- `cargo clippy -p caco-tui --lib` (tj-0df62725): PASSED, 0 warnings.
- All changed files rustfmt-clean (app.rs inserted regions verified via skip_children reformat diff — pre-existing whole-file drift untouched); `git diff --check` clean.

## Diff
See the reintegration receipt for the landed squash SHA.
