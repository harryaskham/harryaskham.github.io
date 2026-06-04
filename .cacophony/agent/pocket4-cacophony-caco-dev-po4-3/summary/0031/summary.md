# S3: daemon `/suggest` generate endpoint (bd-a4cca4)

## Bead
bd-a4cca4 — caco suggest S3: daemon `/suggest` generate endpoint (structured
LLM, never executes). Slice 3 of the `caco suggest` epic (bd-a84d20). Depends on
S2 (context, landed `0ee535753`) + S1 (config, landed `71662e39a`).

## What changed
New `crates/caco-daemon/src/suggest/endpoint.rs`, registered as
`pub mod endpoint;` in `crates/caco-daemon/src/suggest/mod.rs`, and wired as
`GET`/`POST /api/v1/suggest` into the daemon Router (all listener tables).

The handler `handle_suggest_generate`:
1. Builds scope-aware context inputs from `DaemonState` (nodes + projects
   inventory; richer history slices left to follow-up wiring without changing the
   pure gatherer contract).
2. Calls S2's `gather_context(scope, inputs)` — bounded, oldest-first, 48 KiB.
3. Resolves the configured `suggest.model` alias (S1) via `llm::resolve_endpoint`
   (default `"smart"`).
4. Calls `llm::call_structured` with the suggestion-item JSON schema
   (`name`+`reason`+`type` required; `allow_multiple_runs` optional).
5. Server-assigns a `set-<uuid>` + per-option `opt-<uuid>` ids.
6. Persists a minimal forward-compatible `set.json` + `context.json` under
   `<root>/suggest/<uuid>/` (best-effort; S5 owns the canonical layout).
7. Returns `{ uuid, scope, prompt, created_at, suggestions, context_truncated }`.

Helpers (all unit-tested): `effective_n` (query > config.default_n > fallback,
clamped 1..=25), `suggest_model_alias`, `suggestion_schema`, `parse_suggestions`
(server uuid assignment + required-field filtering + type defaulting),
`build_user_message`, and the `SUGGEST_SYSTEM_PROMPT` (verbatim name/reason
example + explicit never-executes contract).

## Operator decision honored (Harry) — NODE-LOCAL
Per Harry's epic refinement, `/suggest` is **node-local**: it does NOT
forward-if-not-primary (unlike beads). Suggestions are generated from the local
node's own context against its own daemon, not a replicated/authoritative
resource. The `handles_beads_locally` / `forward_post` hop from the expand
template is intentionally dropped.

## Core invariant honored
**Generating NEVER runs a command.** The endpoint only generates + persists
suggestions to triage; execution is a separate explicit user action (S6 `/run`).
The system prompt states this and the schema captures intent only.

## Key architectural finding — parse_valid boundary
S4's `validate_caco_cli` lives in `caco-cli`, which **depends on `caco-daemon`**.
So the daemon cannot call it to set `parse_valid` without a circular crate
dependency. S3 records the raw item `type`; `parse_valid` annotation for
`caco_cli` items is layered in by the caco-cli-side consumer (the picker/CLI
wrapper, S7) that has the command tree available. This keeps the daemon free of
CLI command-tree coupling — the correct layering. Broadcast to the team for S7/S9.

## Slice boundaries
- **S2 (landed)** owns `gather_context`; S3 only assembles inputs + calls it.
- **S5 (open, bd-8441b2)** owns the full `.cacophony/suggest/<uuid>/` layout,
  `--resume`, `SuggestSetCreated` feed emission + cross-node sharing, and bounded
  full-state run-state sync. S3 writes only a minimal durable record; S5 refines.
- **S4 (landed)** validation is caco-cli-side per the cycle boundary above.

## Tests
9 new unit tests (17 total in the suggest module), all green via the daemon queue
(`RUST_MIN_STACK=33554432 cargo test -p caco-daemon --lib suggest:: -- --test-threads=1`):
- `effective_n_prefers_query_then_config_then_fallback`, `effective_n_clamps_to_bounds`
- `suggest_model_alias_defaults_to_smart`
- `parse_suggestions_assigns_uuids_and_defaults_type`,
  `parse_suggestions_drops_items_missing_required_fields`,
  `parse_suggestions_handles_missing_array`
- `schema_requires_name_reason_type`
- `system_prompt_states_never_executes_and_example`
- `user_message_includes_context_count_and_prompt_addendum`

## SPEC areas
SPEC 6.x daemon endpoint surface; SPEC 8.x LLM substrate reuse
(`resolve_endpoint` / `call_structured` / `StructuredLlmRequest`); epic
bd-a84d20 core invariants (read-only generate, node-local, never executes). No
SPEC contract change — additive daemon endpoint.

## Diff summary
New endpoint module + submodule registration + 4 Router route registrations.
Final landed squash SHA per the reintegration receipt.
