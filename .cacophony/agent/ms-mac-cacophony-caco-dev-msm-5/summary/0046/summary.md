# Session summary 0046 — bd-2c7488: caco fleet metrics (slice 1)

## Goal

Time-series complement to `caco fleet snapshot`: expose
Prometheus text format on `/metrics` so disk/agent/bead trends
become Grafana-graphable instead of relying on narrator
commentary.

## Bead(s)

- `bd-2c7488` slice 1 — exporter + minimal serve.

## Before state

- Tonight's disk-pressure investigation needed time-series but
  the only source was narrator's running speech log.

## After state

- `caco fleet metrics` — one-shot Prometheus text on stdout.
- `caco fleet metrics --json` — JSON envelope wrapping the
  Prometheus body and the underlying snapshot.
- `caco fleet metrics serve --port N` (default 9123) — blocking
  minimal HTTP server on `/metrics` (Content-Type
  `text/plain; version=0.0.4`).

Initial gauge set:
- `caco_fleet_uptime_seconds` (Unix ts at render)
- `caco_fleet_projects` (count)
- `caco_fleet_nodes` (count)
- `caco_fleet_agents{project="..."}`
- `caco_fleet_beads{project="...",status="..."}`
- `caco_fleet_snapshot_errors` (count)

Label values escaped (`\\`, `\"`, `\n`).

## Diff summary

- Commit: `f5c00c4d`.
- Files (1): caco-cli lib.rs (+205 lines).
- `cargo build` and `cargo clippy`: clean.

## Operator-takeaway

Implementation re-shells `caco fleet snapshot --json` so we
inherit collection without duplication. Slice 2 follow-ups
worth filing when needed: per-node disk gauges, sync-error
counters, time-to-claim / time-to-close histograms, and an
axum-backed serve with auth + streaming.
