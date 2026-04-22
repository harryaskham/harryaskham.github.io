# Session 0018 — bd-5d2d83 (caco agent introspect)

## Goal

Slice 1 of `caco agent introspect`: dump the live composed profile
chain + comms scope + mute_broadcasts for a running agent. Operators
can ask "what did you actually compose at startup?" without
hand-walking profile inheritance trees.

`--show profile` is the only section wired in slice 1.
hooks/env/tools deferred to follow-ups.

## Bead(s)

- **bd-5d2d83** — primary.

## Before state

- No `caco agent introspect` command existed.
- Profile-loading logic from bd-e8288c
  (`derive_mute_patterns_from_caller_profile`) was inline; reusing
  it for introspect would have meant duplicating the agent.json →
  profiles_dir → compose chain.

## After state

- New `AGENT_INTROSPECT_ARGS` const advertising `--id` (required)
  and `--show` (optional, defaults to `profile`).
- Registered new `introspect` entry in `AGENT_SUBCOMMANDS` after
  `logs`. `mcp_enabled = true`, `agent_safe = true`,
  `idempotent = true` (read-only).
- Refactor: extracted `load_composed_profile_for_agent_id(id, co)
  -> Option<(Profile, chain, profiles_dir)>` from
  `derive_mute_patterns_from_caller_profile`. The latter is now a
  thin wrapper over the new helper. Single source of truth for the
  agent.json → profiles_dir → compose pipeline.
- New `dispatch_agent_introspect(id, show, json, co)`: rejects
  unsupported `--show` sections explicitly (rather than silent empty),
  errors out clearly on unknown agent ID, otherwise renders:
  - text mode: agent id, composed-name, profiles-dir, ordered
    profile chain, permission-mode, comms.scope,
    comms.mute_broadcasts, mcp_servers.
  - json mode: structured envelope with same fields.
- Wired dispatcher arm `[cmd, sub] if cmd == "agent" && sub ==
  "introspect"`.

## Tests

- New: `agent_introspect_help_json_advertises_id_and_show` — locks
  help-json contract for MCP discoverability.
- New: `agent_introspect_rejects_unsupported_show_section` — locks
  the explicit-error behaviour for unimplemented `--show` values.
- New: `load_composed_profile_returns_none_for_unknown_agent` —
  best-effort helper contract.
- All 4 pre-existing derive_mute / doctor_strict tests still pass.

## Validation

- `cargo test -p caco-cli --lib agent_introspect`: 2/2 PASS.
- `cargo test-small`: 4257+ tests across 8 binaries, 0 failures.
- `cargo clippy -p caco-cli -p caco-daemon -p caco-beads -p caco-web
  -p caco-profile --all-targets -- -D warnings`: clean.

## Inline broken-on-main absorbed

- bd-12381f added `self_nudge_interval_secs: Option<u64>` to
  `caco_profile::Profile` but missed a fixture in
  `crates/caco-cli/src/lib.rs` line 79775
  (`reintegration_checks_env` test). Added one-line `:None,`.

## Diff summary

```
crates/caco-cli/src/lib.rs                       | ~+200 / -85
.cacophony/agent/.../summary/0018                | (new)
```

## Operator-takeaway

```bash
# Ask a running agent what it composed:
caco agent introspect --id ms-mac-cacophony-caco-dev-msm-1
caco agent introspect --id ms-mac-cacophony-caco-doctor-msm --json | jq

# Diagnose 'why isn't the mute working?':
caco agent introspect --id <id> | grep mute_broadcasts
```

`--show hooks` / `--show env` / `--show tools` deferred to follow-ups
— file as bd-5d2d83 children when needed.

## Coordination

- Spoke claim with planned scope.
- Will speak completion + reintegrate.

## Notes for next time

- The profile-resolution refactor pattern (extract a shared
  `load_composed_profile_for_agent_id` from a single inline call site
  the moment a second caller appears) is a good rule of thumb. Avoid
  copy-paste; even one extra reader is enough to pay for the
  extraction.
- Pre-existing-fixture patterns: when `cargo check` reports a single
  E0063 in a test fixture, 1-line edit + immediate clippy-recheck
  takes <1min. File a follow-up bead only if the cascade is wide
  (5+ sites).
