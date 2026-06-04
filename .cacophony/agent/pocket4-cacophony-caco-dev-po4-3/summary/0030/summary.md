# S2: `suggest::gather_context` bounded scope-aware context gatherer (bd-98a674)

## Bead
bd-98a674 — caco suggest S2: `suggest::gather_context` bounded scope-aware
context gatherer. Slice 2 of the `caco suggest` epic (bd-a84d20).
Depends on S1 (bd-cc405b, config) — landed first at `71662e39a`.

## What changed
New daemon module `crates/caco-daemon/src/suggest/mod.rs` (directory form, the
agreed home for the growing epic: S2 gather_context + S3 generate + S4
parse-validate). Registered as `pub mod suggest;` in `crates/caco-daemon/src/lib.rs`.

The module owns the **bounded, scope-aware context gatherer** that feeds the
`caco suggest` LLM generator (S3):

- `SuggestScope { project: Option<String>, node: Option<String> }` — narrows
  *what context is gathered*, never the action space (suggested operations stay
  global). Unset dimensions match everything; a set project scope excludes
  project-less rows.
- Compact, serializable context row types: `ContextNode`, `ContextProject`,
  `ContextBead`, `ContextMessage`, `ContextFeedEvent`, `ContextAgent` (agent
  inventory carries `muted` mute state).
- `SuggestContextInputs` — already-fetched input slices (nodes, projects,
  recent beads, messages, feed events, agents). Keeps the assembler pure; the
  daemon I/O that reads these out of `DaemonState` lives in S3.
- `SuggestContext` — the assembled serializable blob (ready for S5's
  `context.json`), carrying `truncated` + `byte_budget`.
- `gather_context(scope, inputs)` — the documented epic entry point; thin pure
  wrapper over `assemble_suggest_context(scope, inputs, budget)` using
  `DEFAULT_SUGGEST_CONTEXT_BYTE_BUDGET` (48 KiB).

### Assembly semantics (pure + deterministic)
1. Scope-filter every section by project/node where applicable.
2. Sort time-ordered sections (beads/messages/feed) **newest-first** by
   timestamp, with id as a stable tiebreaker; static inventory (nodes/projects)
   sorts by name/id. Same inputs → same output (fixture-testable).
3. Clip oversized message bodies to `MAX_MESSAGE_BODY_EXCERPT_BYTES` (512) so one
   huge message cannot dominate.
4. Greedily drop the **oldest** rows across the time-ordered sections until the
   JSON-serialized context fits within the byte budget; mark `truncated`. A
   read-only gather never errors — if even static inventory exceeds budget it is
   returned anyway, marked truncated.

## Invariants honored
- **Read-only gather** — nothing here executes or mutates; it only assembles
  already-fetched data. (Central epic safety contract.)
- **Context scope only** — scope is an input filter, never an execution
  constraint.
- **Bounded + deterministic** — hard byte budget, oldest-first truncation,
  stable ordering.

## Decoupling
Built fully decoupled from S1's `SuggestConfig`: `gather_context` takes a scope +
input slices, not config, so S2 and S1 landed independently with zero conflict.
S3 (the `/suggest` endpoint) is the slice that reads `config.suggest` for the
default budget/n and wires the daemon state into `SuggestContextInputs`.

## Tests
8 unit tests in-module, all green via the daemon queue
(`RUST_MIN_STACK=33554432 cargo test -p caco-daemon --lib suggest:: -- --test-threads=1`):
- `unscoped_context_keeps_all_rows_and_sorts_newest_first`
- `project_scope_filters_other_projects`
- `node_scope_filters_feed_and_agents`
- `budget_truncates_oldest_first` (tiny budget keeps newest, drops oldest)
- `long_message_body_is_clipped`
- `assembly_is_deterministic` (equal-timestamp id tiebreak)
- `scope_matchers_handle_none_dimensions`
- `gather_context_entry_uses_default_budget`

## Coordination
- S1 (po4-1, bd-cc405b) landed first at `71662e39a`; rebased onto it.
- Picked up po4-2's inline broken-on-main clippy fix (bd-50c77e, `4aa18a1fe`,
  caco-cli lib.rs:88051 `sort_by_key`) so the `clippy --workspace` reintegration
  gate is green.
- Slice ownership confirmed by po4-2: S1 po4-1, **S2 po4-3**, S4 done.

## SPEC areas
SPEC 6.x daemon module surface; epic bd-a84d20 core invariants (read-only,
scope-only, bounded). No SPEC contract change — additive daemon module.

## Diff summary
New module + one `pub mod` registration line. Final landed squash SHA per the
reintegration receipt.
