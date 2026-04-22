# Session summary 0000 — bd-940284: caco msg send --strict-target

## Goal

Stop `caco msg send` from silently accepting nonsense `--target`
strings. Operator probe found
`caco msg send --target nonexistent-target --body hi` reports
success even though no inbox row gets created. Add an opt-in
`--strict-target` that pre-flights the target against the
project's known agents before persisting the message.

## Bead(s)

- `bd-940284` — opt-in pre-flight only (slice 1).

## Before state

- `caco msg send --target nonexistent-target ...` returned
  `sent message msg-... to nonexistent-target` with no warning.
- No client-side enumeration of valid targets.

## After state

- New `--strict-target` flag on `caco msg send`.
- When set, `validate_target_in_project(project, target)` queries
  `/api/v1/projects/{project}/agents` and:
  - Allows `cacophony:operator` / `<project>:operator` literally
    (not in agents list but routed by daemon).
  - Returns `Ok(())` if target appears in agent IDs.
  - Errors with close substring matches (up to 3) on miss.
  - Fails open with stderr warning if daemon returns nothing
    parsable (don't block operator on 5xx).
- New `parse_agent_ids` helper handles both `id` and `agent_id`
  JSON field names.
- 3 new unit tests in `tests::parse_agent_ids_*` pass.
- Existing behaviour without the flag is unchanged (bootstrap
  and about-to-spawn flows preserved).

## Diff summary

- Files (1): `crates/caco-cli/src/lib.rs` (+126 lines).
- Tests: +3 (parse_agent_ids_extracts_id_field,
  parse_agent_ids_falls_back_to_agent_id_field,
  parse_agent_ids_returns_empty_on_missing_array).
- `cargo build -p caco-cli` and `cargo clippy -p caco-cli`: clean.

## Operator-takeaway

Workflows that want CLI-level safety can now opt into it:
`caco msg send --strict-target --target X --body Y`. Slice 2
(broader caller-set enumeration: broadcast tags, per-project
aliases via `/api/v1/projects/{project}/callers`) is deferred
until that endpoint exists.
