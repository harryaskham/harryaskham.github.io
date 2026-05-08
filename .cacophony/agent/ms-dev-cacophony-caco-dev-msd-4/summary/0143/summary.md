# Session summary — replication merge stderr recurrence

## Goal

Fix the post-`bd-e1ce96` recurrence where `daemon-crash.log` still received routine replication merge summaries, especially `bd-f769bb` full-state replication hit-rate diagnostics.

## Bead(s)

- `bd-1a8412` — Cluster diagnostics still write to daemon-crash.log after bd-e1ce96 close

## Before state

- Failing tests: none known for this bead.
- Relevant metrics: ms-mac log-monitor observed `daemon-crash.log` grow by 61,057 bytes during a healthy daemon/service window.
- Context: the crash-log tail still included routine `bd-f769bb` replication/push merge summaries, plus already-covered state-pull, full-state push, TLS, peer materialisation, and model-discovery diagnostics.

## After state

- Failing tests: none known for this bead.
- Relevant metrics: focused sidecar stderr-router tests now assert `replication merge from` and slash-style `replication/push merge` / `replication/pull merge` summaries route to `daemon.log`, not `daemon-crash.log`.
- Context: the daemon stderr classifier recognizes replication merge summary variants as non-crash operational diagnostics while preserving conservative crash routing for unknown stderr.

## Diff summary

- Commits: `43afbf9fd9`, `0aa3d6d92b`
- Files touched: `crates/caco-sidecar/src/lifecycle.rs`
- Tests: added `daemon_stderr_router_replication_merge_summaries_are_diagnostics_bd_1a8412`.
- Behavioural delta: `bd-f769bb` replication merge hit-rate summaries now stay out of `daemon-crash.log`.
- Validation: `tj-683bd394` passed `cargo test -p caco-sidecar bd_1a8412 -- --nocapture`; `tj-ac5adf95` passed `cargo test -p caco-sidecar daemon_stderr_router -- --nocapture`; `tj-2088b054` and `tj-7c6c9e4a` passed the same focused subset after rebases.

## Operator-takeaway

The newest recurrence was another missing stderr phrase: replication hit-rate merge summaries are normal operational telemetry, so they now route with daemon diagnostics rather than crash evidence.
