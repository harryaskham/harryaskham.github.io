# Session summary — bundled cacophony tmux config for managed agent sessions

## Goal

Managed agent tmux sessions inherited whatever `tmux.conf` the host happened to
have (or none), so the agent statusline, leader key, and graphics passthrough
were inconsistent across local / Codespaces / other environments. This session
adds a bundled, Cacophony-specific tmux configuration and wires managed agent
launch to use it instead of the host config — giving every agent a consistent
statusline (window list + agent locator) and QOL setup regardless of host.

## Bead(s)

- `bd-d7ec99` — Create bundled cacophony-specific tmux.conf (P2 feature)
- `bd-760430` — Configure agents to use bundled tmux.conf instead of system config (P2 task)

Claimed and implemented as a tight create→wire batch.

## Before state

- Agents launched `tmux new-session` with no `-f`/bundled config, inheriting the
  host system/user `tmux.conf`.
- No `tui.tmux` config surface existed.
- Failing tests: none (pre-existing reintegration::tests broken-on-main bd-9f33ab
  is unrelated and owned by aur-2).

## After state

- New `tui.tmux` config (`TuiTmuxConfig { config: Option<String> }`) plus a
  bundled `DEFAULT_BUNDLED_TMUX_CONFIG` constant providing the required
  statusline + QOL contract.
- `TuiConfig::effective_tmux_config()` resolves the effective text; the spawn
  path materializes it per agent and sources it onto the tmux session.
- Absent `tui.tmux` => unchanged (host tmux.conf applies).
- Validation: 4 new resolver/overlay tests pass; caco-config schema tests pass;
  caco-daemon compiles clean; clippy clean on caco-config + caco-daemon;
  regenerated config-schema docs are up-to-date (`--check` passes).
- Failing tests: none introduced.

## Diff summary

- Code/content commit: `c1fa888008` (final landed squash SHA from the reintegration receipt).
- Files touched: `crates/caco-config/src/model.rs` (schema + struct + resolver),
  `crates/caco-config/tests/config.rs` (+4 tests, +6 exhaustive-literal fixups),
  `crates/caco-daemon/src/agent/types.rs` (AgentCreateRequest.tmux_config),
  `crates/caco-daemon/src/agent/lifecycle.rs` (materialize tmux.conf + locator env),
  `crates/caco-daemon/src/agent/health.rs` (source-file onto session + locator env allowlist),
  `crates/caco-daemon/src/modes.rs` and `src/lib.rs` (resolve config at spawn),
  `crates/caco-daemon/src/{spawn_routing,test_bridge}.rs` + `agent/tests.rs` (None field fixups),
  `crates/caco-cli/src/lib.rs` (None field fixup),
  `docs/config-schema/{index,tui-21}.html` (regenerated), `AGENTS.md` (contract bullet).
- Tests: +4 / -0 / flipped 0.
- Behavioural delta: when `tui.tmux` is configured, managed agents launch with the
  bundled statusline (tiny window list left, `@<agent-id>` locator right,
  transparent bg, blank window-status-separator) and QOL (C-a leader, extended-keys,
  passthrough graphics), sourced per session after `new-session`.

## Operator-takeaway

There is now a first-class `tui.tmux` config surface: set `tui.tmux: {}` to opt
every managed agent on a node into the bundled Cacophony tmux statusline + QOL,
or provide `tui.tmux.config: |` with your own literal to fully override it.
Because managed agents share one tmux server, the bundled config is applied via
`tmux source-file` per session (not `tmux -f`, which only fires at server start),
so the statusline materializes deterministically on every agent's startup and the
right side shows `@<agent-id>` from the `CACO_AGENT_LOCATOR` session env var.
