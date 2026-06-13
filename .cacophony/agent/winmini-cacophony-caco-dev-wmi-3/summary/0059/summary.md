# bd-4fbd23 — accept --no-gfx in caco agent attach

## Bead
bd-4fbd23 (agent-attach/caco/cli/tui, P2 feature; filer harryaskham). `caco agent attach --no-gfx` was unrecognized → warning "received unrecognised flag(s): --no-gfx". AC: accept --no-gfx without warning, suppress graphics in the spawned standalone chrome, consistent with other TUI-capable commands (`caco tui --no-gfx`).

## Fix (crates/caco-cli/src/lib.rs)
The standalone agent chrome is built in-process via `caco_tui::App::new(...)` in `dispatch_agent_wrapper_tui`, whose 8th arg is `no_gfx` (force ASCII-only / disable kitty graphics) — it was hardcoded `false`. Threaded the flag through:
- `AGENT_ATTACH_ARGS`: added the `--no-gfx` ArgSpec (so it is recognized — no "unrecognised flag" warning; AC 1+4).
- Dispatch site (`agent attach` arm): parse `no_gfx = parsed.flags.contains_key("--no-gfx")` and pass it down.
- `dispatch_agent_attach(..., no_gfx, ...)` → `dispatch_agent_wrapper_tui(..., no_gfx, ...)` → `App::new(... no_gfx ...)` (AC 2+3, consistent with `caco tui --no-gfx`).
- Updated the other 3 `dispatch_agent_wrapper_tui` callers (other attach entry points) to pass `false`, preserving their current behavior (out of this bead's scope).
- `--raw`/JSON attach returns before the chrome, so `--no-gfx` is accepted there but a no-op (no warning).

## Validation (daemon test queue, --cwd at checkout)
- `cargo clippy -p caco-cli --lib` (tj-8187e357): exit 0, clean.
- `cargo test -p caco-cli --lib agent_attach_args_include_no_gfx` (tj-6e69cea2): 1 passed (first run hit a `daemon_restart_recovered` infra error from an operator `caco update --restart` mid-test; re-ran clean).
- New arg-spec test `agent_attach_args_include_no_gfx_bd_4fbd23`. rustfmt-clean; git diff --check clean. AGENTS.md managed-attach contract updated.

## Diff
See reintegration receipt for the landed squash SHA.
