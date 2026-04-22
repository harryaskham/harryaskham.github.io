# Session summary — bd-205791 slice 1: PersistentAgentDecl.depends_on_node

## Goal

Land the data-model surface for cross-node reachability dependencies on
controller-style persistent agents, so operators can declare that a
persistent's launch should be deferred until a named remote node is
reachable. The reconcile-side gating wires through in a follow-up
slice.

## Bead(s)

- `bd-205791` (slice 1 of N) — Cross-node controller persistents:
  gate launch on `PeerReachabilityMap[remote_node]`. Filed as a
  refinement of bd-cc441e after my prior triage note showed the
  original framing didn't match `Config::resolve_persistent_agents`
  reality.

## Before state

- `PersistentAgentDecl` had no field for declaring a cross-node
  dependency.
- Cross-node controllers (e.g. caco-doctor controllers polling
  remote /api/v1/...) silently failed their `init.sh` step when the
  remote node was unreachable; `restart_failures` grew and the agent
  ended up in `Failed` with a confusing `init.sh exit 1` last_error.
- The pre-existing 76 struct-literal constructions of
  `PersistentAgentDecl` in tests/fixtures had no path to declare the
  dependency.

## After state

- `PersistentAgentDecl.depends_on_node: Option<String>` added with
  `#[serde(default, skip_serializing_if = "Option::is_none")]` so
  existing configs round-trip byte-identical when the field is unset.
- `compile_checked_fields!(PersistentAgentDecl { ... })` schema
  block lists the field; `leaf("depends_on_node", ...)` documents
  it in the schema-doc array consumed by `caco config schema`.
- Bulk-edit (Python regex over `project: None,\s*\n\s*}`): 76
  `PersistentAgentDecl` constructions across
  `crates/caco-daemon/src/persistent.rs` (74) and
  `crates/caco-config/src/validate.rs` (2) gained
  `depends_on_node: None,` so the struct literal stays exhaustive.
- New tests in `crates/caco-config/src/model.rs::persistent_agent_decl_tests`:
  - `persistent_agent_decl_depends_on_node_round_trip`: yaml →
    `PersistentAgentDecl` → yaml preserves the field.
  - `persistent_agent_decl_depends_on_node_default_none`: omitting
    the field yields None and serialization skips it (so existing
    YAML files round-trip without spurious diffs).
- `cargo test -p caco-config --lib`: 718 passed. `cargo test -p
  caco-daemon --lib persistent`: 127 passed; 2 failures
  (`persistent_recreate_relaunches_project_controller_replacement`
  and `running_persistent_agent_recreate_forces_destructive_relaunch`)
  are PRE-EXISTING per msm-2's broken-on-main note (verified by
  stashing and re-running on plain HEAD).
- `cargo clippy -p caco-config -p caco-daemon --tests` clean.

## Diff summary

- Commit: `d1fd703b`
- Files touched (3):
  - `crates/caco-config/src/model.rs` — new field, schema entry,
    new tests module (+~120 lines).
  - `crates/caco-daemon/src/persistent.rs` — 74 fixture sites
    updated with `depends_on_node: None,`.
  - `crates/caco-config/src/validate.rs` — 2 fixture sites updated.
- Tests: +2.
- Behavioural delta: zero for runtime — the field is read by no
  code path yet. YAML configs may now declare `depends_on_node`
  without parse errors or schema warnings.

## Out of scope (next slice on same bead)

- **Reconcile-side gating** (the actual behaviour change):
  `PersistentSentinel::reconcile` must consult
  `PeerReachabilityMap[depends_on_node]` before pushing a
  `start_action`, set `last_error =
  "depends_on_node <name> unreachable"`, and explicitly NOT advance
  `restart_failures` (unreachability is not the agent's fault).
  Threading `peer_reachability` through `reconcile` and its ~10
  callsites is the bulk of the next slice.
- caco doctor surfacing of "waiting for remote node X" as a
  distinct status (vs `pending`/`failed`).
- TUI agent detail "waiting for remote node X" annotation.

bd-205791 stays in_progress with my assignment so the next slice
lands on the same bead.

## Operator-takeaway

`depends_on_node: <peer-node-name>` is now a valid field in any
persistent agent declaration. It has no runtime effect yet — the
reconcile gate will land in the next slice. Until then, the field
serves as a forward-compat marker for configs that want to be
ready when the gate lands.
