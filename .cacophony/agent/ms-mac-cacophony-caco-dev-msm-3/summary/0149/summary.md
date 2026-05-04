# bd-a98020 profile/config agent-defaults alignment

## Bead
- `bd-a98020` — Align agent profile frontmatter with config agent-defaults schema.

## Changes
- Added `PROFILE_FRONTMATTER_FIELDS` in `crates/caco-profile/src/model.rs` and compile-coverage test `profile_frontmatter_field_list_covers_profile_struct_bd_a98020` so new profile frontmatter fields require an explicit contract update.
- Added canonical serialized key lists in `crates/caco-config/src/model.rs`:
  - `AGENT_DEFAULTS_KEYS`
  - `PERSISTENT_AGENT_DECL_KEYS`
- Reused those key lists in unknown-key validation so `agent_defaults` and persistent declarations cannot drift from schema/model support.
- Fixed generated schema placement:
  - `isolation` remains accepted and documented under `projects[].agent_defaults`.
  - `project` was removed from `projects[].agent_defaults` and documented under persistent agent declarations instead.
- Added regression coverage that compares schema children with serialized key contracts and classifies every profile/config key as shared, config-only/declaration-only, or profile-only.
- Updated operator-facing docs in `SPEC.md`, `README.md`, and `AGENTS.md` to clarify the profile/frontmatter vs project `agent_defaults` vs persistent declaration boundary, including the daemon HTTP timeout distinction.
- Regenerated generated docs/artifacts affected by the profile/schema contract:
  - `docs/config-schema/*`
  - `docs/profiles.html`
  - `plugins/caco-agent/agents/project-controller.md`
  - `plugins/caco-agent/agents/repo-health.md`

## Validation
After the operator no-raw-cargo instruction, remaining validation was run only through first-party Cacophony queues:

- `tj-b2a019b4` — `cargo test -p caco-profile bd_a98020 -- --test-threads=1` passed.
- `tj-b3c3b410` — `cargo test -p caco-config bd_a98020 -- --test-threads=1` passed.
- `tj-d3ebe617` — `cargo test -p caco-profile --test profile controller_profile -- --test-threads=1` passed.
- `tj-78449527` — generated-docs check hit retryable daemon-restart recovery; no test failure.
- `tj-78bb0f54` — retry of generated-docs check passed:
  - `cargo run -p caco-config --bin caco-config-schema-docs-gen -- --check`
  - `cargo run -p caco-profile --bin caco-docs-gen -- --check`
- `bj-c8af58e9` — queued `cargo clippy -p caco-profile -p caco-config --lib --no-deps -- -D warnings` passed before the final persistent-declaration classification tightening.
- `tj-3587ff98` — queued regression rerun for `profile_agent_defaults_alignment_decisions_are_exhaustive_bd_a98020` passed after the tightening.
- `bj-d7b0459f` — queued `cargo fmt --all -- --check` passed.
- `bj-1e208e92` — queued `cargo clippy -p caco-config --lib --no-deps -- -D warnings` passed after the final config test edit.

## Rebase validation
- Direct reintegration initially refused the branch as stale after main advanced; recovered with `caco agent rebase --id $CACO_AGENT_ID`.
- Resolved the generated `docs/config-schema/index.html` total-field-count conflict by keeping the regenerated count matching the section pages.
- Re-ran bounded queued validation on the rebased branch:
  - `tj-c7cda6f3` — `cargo test -p caco-profile bd_a98020 -- --test-threads=1` passed.
  - `tj-fc524480` — `cargo test -p caco-config bd_a98020 -- --test-threads=1` passed.
  - `tj-bfdbe6cd` — generated docs checks passed:
    - `cargo run -p caco-config --bin caco-config-schema-docs-gen -- --check`
    - `cargo run -p caco-profile --bin caco-docs-gen -- --check`
