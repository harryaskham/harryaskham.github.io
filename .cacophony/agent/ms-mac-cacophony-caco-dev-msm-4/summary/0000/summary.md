# Session summary — bd-a2bc19 planned node lifecycle broadcasts

## Goal
Add the first daemon-side slice for planned node outages so a node can announce expected downtime or recovery before peers treat it as an unexpected unreachable failure.

## Bead(s)

- `bd-a2bc19` — Per-node lifecycle event hooks: last-gasp broadcasts on shutdown/sleep/battery so peers suppress error noise

## Before state

- Nodes had no first-party lifecycle broadcast primitive for shutdown, sleep, update, or battery-critical states.
- The daemon had no runtime/planned-outage read model to attach expected downtime to node discovery responses.
- Operators and controllers had to infer whether an unreachable node was expected from external context.

## After state

- Added `node_lifecycle` feed events and a `POST /api/v1/daemon/lifecycle` endpoint.
- Added `caco daemon lifecycle [state] --reason ... --ttl-secs ...` as the CLI hook surface.
- Added a `planned_outages` SQLite table plus in-memory map, materialized on startup and on ingested lifecycle events.
- `/api/v1/nodes` and `/api/v1/nodes/<node>` now include `planned_outage` when a node has an unexpired expected-downtime hint.

## Diff summary

- Commit: `0e09dda88` after replay onto the remote agent branch.
- Files touched: `crates/caco-cli/src/lib.rs`, `crates/caco-daemon/src/feed.rs`, `crates/caco-daemon/src/lib.rs`, `crates/caco-daemon/src/store.rs`.
- Tests: added `store::tests::planned_outage_round_trips_and_clears`.
- Validation: `cargo test -p caco-daemon planned_outage_round_trips_and_clears --lib`; `cargo check -p caco-daemon --tests`; `cargo check -p caco-cli --tests`; `cargo fmt --all -- --check`.
- Behavioural delta: lifecycle-aware nodes can now broadcast expected downtime and peers expose that expected outage in node discovery responses.

## Operator-takeaway

This lands the canonical state and broadcast seam for calmer planned downtime; platform-specific shutdown/battery hooks can now call `caco daemon lifecycle` rather than inventing ad hoc messages.
