# Session summary — Daemon token-usage sink + collection + endpoint (bd-b18111 stage B)

## Goal

Land the durable daemon-side token-usage pipeline that bd-2da2c5 slice 2 calls
for: a daemon sink, a collection loop, and per-project aggregation exposed for
the unified stats aggregate API (slice 3). Built on the shared caco-stats core
(stage A) and the v3 parser fix (stage A.5) so the daemon reuses the same model
and reader instead of duplicating them.

## Bead(s)

- `bd-b18111` — [bd-2da2c5 s2] Daemon+agents: token-usage emission + collection sink + per-project aggregation
- (parent epic: `bd-2da2c5` — unified caco stats; foundation: `bd-9b88a4`)

## Before state

- Failing tests: none.
- No token-usage sink anywhere in caco-daemon (grep: zero token_usage/total_tokens
  fields). Token data was only reachable via the on-demand node-local `caco node
  tokens` CLI scan; nothing durable or queryable by the daemon/API.

## After state

- Failing tests: none. caco-daemon `--lib token_usage` 8/8 pass; daemon lib
  compiles; clippy clean.
- New `crate::token_usage` module:
  - `TokenUsageRecord` + `token_usage` SQLite table keyed by `(node, project)`
    with idempotent cumulative UPSERT (`INSERT ... ON CONFLICT DO UPDATE`).
  - `query_token_usage` (project/node filters, ordered by total), `rollup_by_project`
    and `grand_total` fleet-rollup helpers (work for one node now, many later).
  - Append-only `daemon/state/token-usage.jsonl` snapshot history with bounded
    retention (`MAX_SNAPSHOT_LINES`, ~10 days at the 5-min cadence).
  - `collect_session_token_usage` reads `~/.pi/agent/sessions` via the shared
    caco-stats reader and aggregates per project; `collect_and_persist` ties read
    + UPSERT + snapshot together.
- `store.rs` inits the table at startup alongside perf/exceptions.
- `lib.rs`: a 5-minute background collection loop (initial 60s delay) that scans
  the node's Pi session store off the executor via `spawn_blocking`, UPSERTs
  cumulative per-project totals, and appends a snapshot; plus
  `GET /api/v1/token-usage` (records + per-project rollup + grand total)
  registered on both the local and cluster routers.

## Diff summary

- Code/content commits: `ee1574a0cd` (pending final squash SHA from reintegration receipt)
- Summary artefact commit: intentionally omitted (no self-reference)
- Files touched: `crates/caco-daemon/src/token_usage.rs` (new, +489), `lib.rs`
  (module decl, route x2, handler, collection loop; +129), `store.rs` (init_table),
  `crates/caco-daemon/Cargo.toml` (+caco-stats dep), `Cargo.lock` (dep edge)
- Tests: +8 / -0 / flipped 0
- Behavioural delta: the daemon now durably collects and serves per-project token
  usage; nothing else changes (read-only against session logs, idempotent).

## Operator-takeaway

bd-2da2c5 slice 2's durable token-usage data source now exists as a daemon sink:
a 5-minute loop reads the node's Pi session logs (cumulative, idempotent — no
double-count), persists per-project totals to a SQLite index + append-only JSONL
history, and serves them at `GET /api/v1/token-usage`. Slice 3's aggregate API
can consume this directly. Remaining (separate, non-urgent follow-ups, noted in
the module): cross-node fleet rollup/replication, and per-model/per-agent
granularity (the reader already exposes model, so this is additive).
