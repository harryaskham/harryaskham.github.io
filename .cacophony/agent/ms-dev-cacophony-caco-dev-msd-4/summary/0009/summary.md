# Summary 0009 — bd-83a84d: caco agent log subcommand

## Bead
bd-83a84d (P3, feature) — `caco agent log --id N --tail/--head/--all`
subcommand for ergonomic out-of-band scrollback access.

Follow-up to bd-87f5bf (which raised tmux history-limit to 100k).
Operators previously had to attach + scroll manually to grab pane
history; this surface lets them grab it from the CLI without
disturbing the live session.

## Change

`crates/caco-daemon/src/agent/health.rs`:
- New helper `capture_tmux_pane_content_full(session, socket) -> Option<String>`.
  Uses `tmux capture-pane -p -S -` (single dash → start of history
  buffer) to grab the entire history-limit window. Returns `None`
  on dead socket / failed capture so callers can distinguish that
  from "pane has no output yet" (Some("")).
- Sits next to existing `capture_tmux_pane_content` (50-line) and
  `capture_tmux_pane_content_lines(N)` helpers; same tmux_command_for
  pattern, same Option<String> contract.
- Re-exported via `agent::*` from `crates/caco-daemon/src/agent/mod.rs`
  (existing `pub use health::*`).

`crates/caco-cli/src/lib.rs`:
- New `AGENT_LOG_ARGS` arg spec (--id required, --tail/--head/--all
  optional and mutually exclusive).
- New `CommandSpec` for `agent log` (singular, distinct from existing
  `agent logs` plural which surfaces structured JSONL session logs +
  lifecycle metadata). Comment in spec disambiguates the two surfaces.
  `mcp_enabled: true, agent_safe: true, idempotent: true` — capture is
  read-only and stateless.
- Dispatch arm in the `agent log` match wires --tail/--head/--all
  through to `dispatch_agent_log`.
- New `dispatch_agent_log` function:
  - Locates agent runtime dir by scanning `paths.agents/<project>/<id>`.
  - Reads `agent.json` for `tmux_session` + `tmux_socket`.
  - Mode resolution (precedence --all > --head > --tail; default
    --tail 100).
  - --tail uses bounded `capture_tmux_pane_content_lines(K)` (cheap).
  - --all and --head use `capture_tmux_pane_content_full`; --head
    slices the first K lines locally.
  - 0-count rejected (`--tail 0` would be useless).
  - JSON output: `{ok, id, mode, requested_lines, captured_lines, content}`.
  - Plain output: raw pane content, or a clear "tmux pane is empty
    or session is dead (session=..., socket=...)" message that names
    the session for operator triage.
  - Local-only for now; remote forwarding deferred to a follow-up
    bead (the local case covers the immediate operator workflow).

## Tests

`crates/caco-daemon/src/agent/tests.rs`:
- `capture_tmux_pane_content_full_returns_none_for_missing_session` —
  confirms the new helper distinguishes dead-socket from empty-pane
  (returns None for missing).

End-to-end smoke (manual via dev binary):
- `./target/debug/caco agent log --help` — help text renders all
  three flags correctly with summaries.
- `./target/debug/caco agent log --id ms-dev-cacophony-caco-dev-msd-4 --tail 5`
  returned the live agent's last 5 pane lines (verified — captured
  rendered TTY frame including spinner + status bar).

## Verification

- `cargo check -p caco-cli` — clean.
- `cargo test -p caco-daemon --lib capture_tmux_pane_content_full` — green.
- `cargo test-small` — 4200/4200 green
  (197+109+720+291+18+2813+52 across small-suite crates).
- Manual end-to-end against this agent's own tmux session — works.

## Operational impact

- New CLI surface, no behaviour change to existing commands.
- `mcp_enabled: true` — TUI + MCP can call it.
- `agent_safe: true` — child agents can introspect their own pane
  history (or peer agents' if scope allows).
- Local-only initially; remote forwarding deferred. The `agent logs`
  surface already has remote-forwarding plumbing (resolve_daemon_connection_for
  + GET /api/v1/agents/{id}/logs); the equivalent endpoint for
  `agent log` is a clean follow-up.

## Deferred (not in this bead)

- `--since TS`: tmux capture-pane has no native per-line timestamps;
  needs separate design (e.g. parse pane-age stamps or wrap content
  in lifecycle session log timestamps).
- Daemon-side endpoint + remote forwarding: clean follow-up; new
  helper is already public so the daemon route can be a thin wrapper.

## Next

Reintegrate direct, close bd-83a84d, idle.
