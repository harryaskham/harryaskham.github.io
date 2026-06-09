# bd-b95687 — Picophony: wire PicoPane into caco-tui AgentDetail (reachable in-UI pico attach)

## Bead
bd-b95687 (caco-tui/picophony, P2) — parent epic bd-93f302, follow-on to M5 (bd-620edf). Aurora (aur-2) built + landed the `PicoPane`/`AttachSession` capability and explicitly handed this caco-tui integration to me (option (a)) with detailed interface pointers. Coordinated throughout.

## Goal
Make the already-delivered native pico attach pane reachable in the running TUI: render `PicoPane` inside the AgentDetail **Session** inner tab for agents whose `agent_type == "pico"`, connecting an `AttachSession` to `<agent_dir>/pico.sock` lazily on first view, routing keys to `PicoPane::on_key` → async `AttachSession` calls. Additive and parallel to the existing `/pty` attach; non-pico agents and other inner tabs untouched. No new `ContentPane` variant (avoids ~1300 references).

## Change
### crates/caco-tui/src/state/mod.rs
- `TuiState.agent_pico_panes: HashMap<String, PicoPane>` — native attach panes keyed by agent id.
- `TuiState.agent_pico_pending: HashMap<String, Arc<Mutex<Option<Arc<AttachSession>>>>>` — background connect slots; the connect task fills the slot, the event loop polls it into the pane. Both initialized empty in the constructor. (TuiState has no Clone derive, so the non-Clone PicoPane field is safe.)

### crates/caco-tui/src/app.rs
- `pico_socket_for(agent_id)` — resolves the local pico host socket `<agent_dir>/pico.sock` (agent_dir = parent of `checkout_path`, with a `runtime_dir()/agents/<project>/<id>/pico.sock` fallback), matching the `--host` the daemon injects. Local-only first cut.
- `ensure_pico_session(agent_id)` — no-op unless pico; lazily creates `PicoPane::new(socket)` + spawns `AttachSession::connect(socket, None)` on the TUI tokio runtime into a shared slot; polls a completed connect into the pane via `set_session`. The host pi keeps running for other viewers.
- Tick hook: when the AgentDetail Session tab is focused, calls `ensure_pico_session(agent_id)` (gated to pico inside).
- `handle_content_key`: when the Session tab of a pico agent is active and a pane exists, routes the key to `PicoPane::on_key` and runs the `PaneAction` async — `Send` → `prompt`/`steer` (chosen by `is_streaming()`), `Abort` → `abort`, `Detach` → drop the pane+pending (host+pi keep running), `None` → input edit only. A scoped block computes the outcome before the borrow ends, so the spawn/remove avoid borrow conflicts. Placed after button-focus activation so focused detail buttons still win.

### crates/caco-tui/src/views/agent_detail.rs
- AgentDetail Session dispatch: for `agent_type == "pico"`, renders `PicoPane::render(frame, content_area)` when a pane is present; falls back to the existing session-transcript artefact list until the lazy connect completes. Non-pico path unchanged.

## Validation
- `cargo check -p caco-tui --lib` (tj-299e9ec3): PASSED.
- `cargo clippy -p caco-tui --lib` (tj-4049bce5): PASSED, 0 warnings (so the `-D warnings` style-lint gate aurora flagged also passes — verified zero warnings touching the new code).
- `cargo test -p caco-tui --lib picophony` (tj-a139fa5a): existing PicoPane unit tests + test compile.
- state/mod.rs + agent_detail.rs rustfmt-clean; app.rs inserted regions rustfmt-clean (verified via skip_children reformat diff; pre-existing whole-file drift untouched). `git diff --check` clean.

## LIVE-TUI VALIDATION GAP (flagged per aurora's request)
This is deep UI integration with inherently low headless testability. Compile + clippy + unit tests pass, but the live render/key-routing confirm — opening a real pico agent's Session tab, seeing the live AgentView render, and driving prompt/steer/abort — requires interactive TUI iteration against a live `pico --host <sock>` socket, which a headless winmini worker cannot drive. **Live confirm falls to aurora/operator** (deploy-gated on a daemon carrying the pico binary, bd-785634-adjacent). Local socket is hardcoded; aurora's M6 (bd-019da7) will expose the pico socket path in attach metadata for remote/peer-forwarded attach — I'll consume that metadata instead of the local-dir derivation when it lands.

## Diff
See the reintegration receipt for the landed squash SHA.
