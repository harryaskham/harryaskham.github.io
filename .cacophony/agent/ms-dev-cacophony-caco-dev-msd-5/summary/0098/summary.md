# Session summary — bd-fce8b8 slice 1c (caco doctor startup-degraded row)

## Goal

Complete the operator-facing surfacing for agent-start errors: in addition to
`caco agent status` (slices 1+1b), `caco doctor` should flag local agents that
started with wrapper.log errors, so a silent degraded start is caught in
post-restart triage without inspecting each agent individually.

## Bead(s)

- `bd-fce8b8` — Agent-start extension/MCP load errors fail silently (slice 1c:
  caco doctor surfacing via a fleet-summary startup_degraded array)

## Before state

- `startup_diagnostics` was surfaced only on the per-agent `caco agent status`
  and logs responses (slices 1+1b). `caco doctor` had no degraded-start check,
  and the fleet summary (`/api/v1/agents/summary`) did not expose the signal.

## After state

- The daemon fleet summary exposes a curated `startup_degraded` array (local
  non-terminal agents carrying a non-null `startup_diagnostics`), computed via
  the slice-1 `detect_startup_diagnostics` helper and bounded to local
  non-terminal agents (consistent with the endpoint's existing per-agent I/O;
  peer agents excluded since their wrapper.log is not local).
- `caco doctor` emits a lifecycle `startup diagnostics` row: `ok` when none, or
  `warning` listing the degraded agents and a triage hint.

## Diff summary

- Code/content commit: `bd-fce8b8: surface startup-degraded agents in caco
  doctor (slice 1c)`. Final landed squash SHA from the receipt.
- Summary artefact commit: intentionally omitted (no self-reference).
- Files touched: `crates/caco-daemon/src/lib.rs` (per-agent
  `local_agent_startup_diagnostics_json`, curated `startup_degraded_entries`
  helper, summary response field), `crates/caco-cli/src/lib.rs` (doctor check).
- Tests: added `startup_degraded_entries_filters_to_agents_with_diagnostics_bd_fce8b8`.
  Validation: queued `cargo check -p caco-daemon -p caco-cli` (10m real compile,
  clean) + the new unit test passed.
- Behavioural delta: `caco doctor` now flags agents that started degraded.

## Operator-takeaway

This completes the `caco doctor` half of bd-fce8b8's surfacing. The detection
remains on-demand (no new sidecar / detect-at-start persistence yet) and bounded
to local non-terminal agents to keep the fleet-summary scan cheap. Remaining
open follow-ons: Pi-format semantic parsing (external Pi error-log format) and
the external agent-utils pi-mcp-adapter vs tendril-share.js tool-name dedup.
