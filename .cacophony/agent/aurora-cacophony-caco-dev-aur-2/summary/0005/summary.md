# Session summary — Re-materialize running Pi settings on default-model change (bd-559fda)

## Goal

Close the runtime-staleness gap from bd-907504: when `default_model.yaml`
(imported as `agent_defaults.model`) flips on main, ALREADY-RUNNING Pi agents
kept a stale `defaultModel`/`enabledModels`/context window in their materialized
`.pi-agent/settings.json` until recreated or manually refreshed — so the
statusline could show one model while requests used another. Auto-trigger
re-materialization on the config hot-reload path.

## Bead(s)

- `bd-559fda` — Re-materialize running Pi agents' settings.json on default_model.yaml change (staleness follow-up to bd-907504)

## Before state

- The config hot-reload path (`config_reload_loop`) swapped the hot config but
  did not re-materialize running agents' Pi settings on a default-model change.
- `caco agent refresh` already re-materializes settings.json/models.json
  (`refresh_profile_artifacts`, lifecycle.rs), but nothing auto-triggered it on
  a config change — the documented gap.

## After state

- New `rematerialize_pi_agents_on_model_change(state, old_config, new_config)`
  in lib.rs: for each running local Pi agent whose effective model
  (`resolve_agent_model_provider`) differs between old and new config, it reuses
  the existing `refresh_profile_artifacts` path to rewrite settings.json/
  models.json with the new model fallback. Only changed agents are touched; no
  disruptive live runtime reload is forced.
- `config_reload_loop` spawns this in a background task right after the hot
  config swap so per-agent I/O does not stall hot-reload.
- `refresh_profile_artifacts` exposed `pub(crate)` for reuse (no logic
  duplication).

## Diff summary

- Code/content commit: pending final squash SHA from reintegration receipt.
- Files touched: `crates/caco-daemon/src/lib.rs` (orchestration fn + test),
  `crates/caco-daemon/src/agent/lifecycle.rs` (visibility),
  `crates/caco-daemon/src/config_reload.rs` (background trigger).
- Tests: +1 detection test
  (`rematerialize_trigger_detects_default_model_change_bd_559fda`); existing
  config_reload (18) and resolve_*_model_provider tests green; clippy clean.
- Behavioural delta: a default-model config change now refreshes running Pi
  agents' materialized settings files automatically.

## Operator-takeaway

The wrong-model statusline window after a `default_model.yaml` flip is now
self-healing for the materialized files: the config hot-reload path detects the
effective-model change per running Pi agent and reuses the existing refresh
path to rewrite settings.json/models.json, without recreating agents or forcing
a disruptive live reload. A live statusline guard (preferring the actual
request/response model) remains the optional agent-utils-side defense-in-depth.
