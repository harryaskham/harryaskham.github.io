# Technical-writer review summary

## Goal

Document the canonical /api/v1/stats unified-stats endpoint (bd-d276e2) in
api.html, the follow-up I committed to after the flip doc-sync.

## Bead(s)

- `bd-d276e2` — canonical /api/v1/stats aggregate (bd-2da2c5 epic slice 3); `bd-0f6909` (compute rollup), `bd-b18111` (tokens).
- `bd-c63005` — technical-writer documentation maintenance.

## Before state

- api.html had no Cluster stats section: neither `/api/v1/stats` nor `/api/v1/cluster/compute` were documented.

## After state

- Added a "Cluster stats" section to api.html with `/api/v1/stats` (canonical unified-stats aggregate; query params project/node/window/bucket) + `/api/v1/cluster/compute`, plus a prose description of the response shape (generated_at, scope, compute ClusterComputeRollup, per_node[], throughput, tokens).
- VERIFIED against source (lib.rs StatsResponse/StatsQuery/handle_stats) — and CORRECTED the agents' slice-A report: `tokens` is now POPULATED (`{by_project, grand_total}`, bd-b18111 s2), not null; only `throughput` remains the stable-nullable follow-up field. validate-pages passed.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `docs/api.html`.
- Behavioural delta: documentation only.

## Operator-takeaway

The unified-stats endpoint is now documented with its real (source-verified)
shape, including that tokens is live and throughput is the remaining stable-null
follow-up field — so the four-frontend contract is clear for consumers.
