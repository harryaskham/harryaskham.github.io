# Session summary — wire async create/spawn lifecycle operations

## Goal

Wire the deferred create/spawn half of the daemon async lifecycle executor so the
TUI/web/mobile render-progress surfaces can drive an agent **create/spawn** to a
terminal phase, not just existing-agent intents. create/spawn target a
not-yet-existing agent, so the per-agent `/agents/{id}/operations` POST is the
wrong entrypoint; this session adds a project-scoped operations surface and the
idempotency-key shape for it.

## Bead(s)

- `bd-fdeb30` — Lifecycle executor: wire create/spawn intents via dedicated
  entrypoint + idempotency-key shape (bd-b43257 follow-up)
- builds on `bd-7dd5ec` (executor `drive_lifecycle_operation`) and `bd-b43257`
  (discard slice); handoff confirmed by the executor author (winmini) and
  Aurora.

## Before state

- Failing tests: none introduced.
- `lifecycle_executor::drive_lifecycle_operation` drove recreate/resume/stop
  (bd-7dd5ec) and discard (bd-b43257); create/spawn/reintegrate/handoff were
  record-only because `lifecycle_operation_running_message` returned `None` and
  the per-agent driver early-returned.
- No project-scoped operations endpoint existed; `AgentCreateBody` had no
  pre-generated `id` passthrough.

## After state

- Failing tests: none.
- New `POST /api/v1/projects/{project}/agents/operations` records a create/spawn
  `LifecycleOperation` against a pre-generated (or caller-supplied) agent id and
  drives `handle_agent_create` (async_spawn forced off) to a terminal phase via a
  dedicated project driver. Existing-agent intents are rejected on this surface;
  create/spawn remain record-only on the per-agent surface.
- `AgentCreateBody` gained an additive `id: Option<String>` that threads into
  `AgentCreateRequest.pre_generated_id`, so the operation record correlates to the
  created agent.
- SPEC §15.2 documents the new surface, idempotency-key shape, and execution
  semantics.

## Diff summary

- Code commits: pending final squash SHA from the reintegration receipt.
- Files touched: `crates/caco-daemon/src/lib.rs` (new
  `lifecycle_operation_project_running_message`, `ProjectLifecycleOperationCreateBody`,
  `spawn_project_lifecycle_operation_driver`,
  `handle_project_agent_lifecycle_operation_create`, route in both router
  builders, additive `AgentCreateBody.id`, `pre_generated_id` threading, focused
  unit test), `SPEC.md` (§15.2 operations surface paragraph).
- Tests: +1 focused unit test
  (`lifecycle_operation_project_running_message_executes_create_spawn_only_bd_fdeb30`).
- Behavioural delta: create/spawn agent launches can now be tracked as durable
  async lifecycle operations through a project-scoped entrypoint.

## Operator-takeaway

create/spawn async lifecycle operations now have a real home: a project-scoped
operations POST that pre-generates the agent id, records the operation, and drives
the create to a terminal phase. The optional "richer intermediate phase events"
item from the bead was intentionally left as a future consider-item; the executor
still emits a single Running marker per operation.
