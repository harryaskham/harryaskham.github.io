# Session summary — bd-346e5d: optional stateful daemon /assistant endpoint (Slice 1, reuse-only)

## Goal
Add an OPTIONAL stateful daemon `/assistant` endpoint backed by a persistent pico session, so the
Android device-Assistant invocation (foundation bd-160b14) can route to a stateful daemon-side
assistant rather than only opening the local chat scope. First `[headless-slice-ready]` bead from
the triage pipeline; daemon-Rust lane.

## Bead(s)
- bd-346e5d (P3 feature). Slice 1 = the contract + handler + session lifecycle reusing the EXISTING
  managed-pico spawn/session machinery, feature-flagged, unit-tested. Slice 2 (android routing) is a
  separate later follow-up (not this work).

## Before state
No `/assistant` endpoint; the device-Assistant could only open the local chat scope. No daemon-side
contract for routing a prompt to a persistent per-scope pico session.

## After state — Slice 1 (REUSE-only) implemented + tested
New `crates/caco-daemon/src/assistant.rs` module + a `POST /api/v1/assistant` handler:
- Contract: `AssistantRequest { scope, message }` / `AssistantResponse { reply, session_agent_id,
  created }`.
- Env feature-flag `CACO_ASSISTANT_ENABLED` (opt-in, default off → 404). A declarative
  `assistant.enabled` config-section flag (schema/docs gate) is a follow-up.
- Pure reply-aggregation `assistant_reply_from_events` (RpcEvent TextDelta → reply until AgentEnd).
- Pure create-or-reuse `resolve_assistant_session` (keyed by scope, live-only — a dead/stopped
  session never satisfies reuse).
- Round-trip helper `assistant_socket_round_trip` reusing `caco_picophony::ViewerClient`
  (connect_unix + submit `HostRequest::Prompt{streamingBehavior:followUp}` + collect
  `HostMessage::Event(RpcEvent)` until AgentEnd/PiExited/timeout → aggregate). Bounded by
  `ASSISTANT_TURN_TIMEOUT_SECS` (120s).
- Handler: env-gate → validate → build candidates from live LOCAL pico agents keyed by
  short_name == scope → resolve → Reuse: socket round-trip → `AssistantResponse`; Create: structured
  `assistant_no_live_session` (auto-spawn-on-create is the Slice-1b follow-up).
- Registered on local_router.

Reuse-only ships a working endpoint for an existing scope-bound pico session while keeping the whole
foundation used (no dead-code). Auto-spawn-on-create + strict turn/id correlation under concurrency
(the host fans events to all viewers) are the Slice-1b finalization (live integration check), per the
bead's "implement+unit-test headless, defer-live" guidance.

## Diff summary
- New crates/caco-daemon/src/assistant.rs (contract + pure logic + round-trip helper + 5 unit tests).
- crates/caco-daemon/src/lib.rs: `pub mod assistant;`, `handle_assistant` handler, `/api/v1/assistant`
  route on local_router.
(Final landed squash SHA: see the reintegration receipt.)

## Validation
- `cargo clippy -p caco-daemon --lib --tests`: clean (no new warnings; the one `unused variable: sub`
  is pre-existing elsewhere).
- `cargo test -p caco-daemon --lib assistant::`: 5/5 pass (env-flag, reply-aggregation, session-reuse).
- Real-cargo `cargo test-small` (echo-gate is disabled) via the queue before land.

## Operator takeaway
The optional stateful `/assistant` endpoint exists (opt-in, default-off). With a live per-scope pico
session (a managed pico agent whose short_name == the scope), a device-Assistant POST routes the
prompt to that session and returns the aggregated reply. Enable with `CACO_ASSISTANT_ENABLED`.
Follow-ups: auto-spawn-on-create (Slice 1b), strict turn correlation, the `assistant.enabled`
config-section flag, and the android assist→endpoint routing (Slice 2).
