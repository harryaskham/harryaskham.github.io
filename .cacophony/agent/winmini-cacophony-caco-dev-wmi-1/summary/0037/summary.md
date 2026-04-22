# Session summary — auto-derive --goal from persistent.yaml (bd-6b2af8)

## Goal

`caco agent new --profile X` against a persistent declaration
with a `goal:` field still required `--goal` because the goal
field was being silently dropped (not modeled in
`PersistentAgentDecl`).

## Bead(s)

- `bd-6b2af8` — caco agent new should auto-derive --goal for
  declarative-persistent spawns from persistent.yaml goal field
  (P3 feature)

## Before state

- `goal:` in `cacophony_persistent.yaml` was silently ignored
  by serde (field missing from `PersistentAgentDecl` struct).
- Operator hit "agent goal is required for non-debug-shell
  launches" even when their persistent decl had `goal:` set.
- Workaround: pass `--goal` at every spawn.

## After state

- `PersistentAgentDecl` gains `goal: Option<String>` field.
- `compile_checked_fields!` macro updated to recognize it.
- Schema entry added to `persistent_agent_children` so
  `caco config schema` documents it.
- `dispatch_agent_new` now resolves goal in this order:
  1. Explicit `--goal` flag (if provided).
  2. Persistent declaration matching `--profile <name>` on
     `--node <name>` (if --node set; otherwise any node) — both
     node-scoped and project-scoped persistent maps searched.
  3. None (daemon will reject with bd-89acd7 "goal required"
     as before).
- Auto-derivation logs to stderr ("info: bd-6b2af8: --goal
  not provided; using goal from persistent declaration 'X'")
  so operators see what was inferred.

## Diff summary

- Files touched (+93 / −2):
  - `crates/caco-config/src/model.rs`: PersistentAgentDecl
    `goal` field + schema leaf + macro check.
  - `crates/caco-config/src/validate.rs`: backfill 2 test
    fixtures (`goal: None`).
  - `crates/caco-cli/src/lib.rs`: resolved_goal lookup before
    req_body construction.

## Verification

- `cargo build --workspace`: clean.
- `cargo test -p caco-config --lib`: 747 pass.
- `cargo clippy -p caco-cli --lib --tests -- -D warnings`: clean.
- `cargo clippy -p caco-config --lib --tests -- -D warnings`: clean.
- One pre-existing test failure
  (`agent_introspect_rejects_unsupported_show_section`) confirmed
  to fail on main without my changes — unrelated (bd-b7a3c5
  env-isolation territory).

## Operator-takeaway

After this lands, `caco agent new --profile test-user-hel
--node helsinki` will automatically pull the goal from
`cacophony_persistent.yaml` instead of erroring. Eliminates
the foot-gun where adding a persistent entry with a goal
required also remembering --goal at every spawn.
