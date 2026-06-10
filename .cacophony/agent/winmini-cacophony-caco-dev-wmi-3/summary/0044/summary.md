# bd-d2cd00 — caco-tui: wire remote pico attach via connect_ws to the daemon /session bridge

## Bead
bd-d2cd00 (caco-tui/picophony, P2) — third increment of the pico-attach arc (after bd-b95687 wiring + bd-34b6cc pico_socket metadata), consuming aurora's M7: the daemon `/api/v1/agents/<id>/session` WS bridge (2a5ea3f802) and `AttachSession::connect_ws` with the auth hook I requested (1feb127eec): `connect_ws(url: &str, token: Option<&str>, seed) -> io::Result<Self>`.

## Problem
bd-b95687 only handled LOCAL pico agents (`AttachSession::connect` to the local Unix socket). Remote/mesh pico agents had no attach path. The daemon's `/session` WS bridges (local) or peer-forwards over mTLS (remote), but it sits under the loopback `bearer_auth_middleware` which is Authorization-header-only — and `connect_ws` originally couldn't set that header (I traced this and aurora added the `token` param).

## Change
### crates/caco-tui/src/client.rs
- `DaemonClient::bearer_token()` accessor (mirrors `base_url()`), so the TUI can pass its bearer token to the WS upgrade.

### crates/caco-tui/src/app.rs
- `PicoTransport { LocalSocket(String), RemoteWs(String) }` + pure helpers:
  - `pico_session_ws_url(base_url, agent_id)` — derives `ws(s)://…/api/v1/agents/<id>/session` from the client HTTP base URL (http→ws, https→wss). The TUI always opens the WS against its LOCAL daemon, which routes transparently.
  - `resolve_pico_transport(pico_socket, is_local, checkout_path, session_ws_url)` — local agents with a daemon-provided socket (or a derivable `<agent_dir>/pico.sock`) use `LocalSocket`; remote agents (and local agents without a resolvable socket) use `RemoteWs`.
- `ensure_pico_session` (bd-b95687) rewritten: now waits for attach metadata (so the local/remote decision + `pico_socket` are known), picks the transport, and spawns `AttachSession::connect(socket)` (local) or `AttachSession::connect_ws(ws_url, Some(token))` (remote). Removed the old `pico_socket_for` (its runtime-dir fallback is superseded by the `RemoteWs` fallback — the local daemon bridges to its own socket). PicoPane/AgentView/key-routing/render unchanged (identical frame protocol over both transports).

## Tests
- `pico_session_ws_url_converts_scheme_bd_d2cd00`: http→ws, https→wss, trailing-slash trim, agent path.
- `resolve_pico_transport_local_vs_remote_bd_d2cd00`: local+metadata-socket → LocalSocket; local+derive → LocalSocket; local+unresolvable → RemoteWs; remote → RemoteWs.
- `resolve_pico_socket_path` (bd-34b6cc) retained + still tested (reused by `resolve_pico_transport`).

## Validation (daemon test queue, --cwd at checkout)
- `cargo test -p caco-tui --lib bd_d2cd00` (tj-f7a52af8): PASSED 2/2 (lib+tests compile).
- `cargo clippy -p caco-tui --lib` (tj-45f1a3d2): PASSED, 0 warnings.
- app.rs inserted regions rustfmt-clean (skip_children reformat diff; pre-existing drift untouched); client.rs clean; `git diff --check` clean.

## LIVE-TUI VALIDATION GAP (flagged per aurora)
Compile + clippy + unit tests pass, but live confirm — opening a REMOTE pico agent's Session tab and seeing the WS bridge authenticate + render the live AgentView + drive prompt/steer/abort — requires interactive TUI iteration against a live remote pico agent, which a headless winmini worker can't drive. Live confirm falls to aurora/operator (folded into the bd-785634 live-smoke pass). This completes remote/mesh pico attach end to end in the TUI.

## Diff
See the reintegration receipt for the landed squash SHA.
