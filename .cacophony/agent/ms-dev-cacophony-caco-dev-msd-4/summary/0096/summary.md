# Session summary — managed caco-web stale asset detection

## Goal

Make stale managed `caco-web` dashboard assets and launcher drift explicit and actionable after package updates, without regressing the existing static external-port, PID-only lifecycle contract for the dashboard service.

## Bead(s)

- `bd-f74047` — Managed caco web serves stale dashboard assets after update

## Before state

- Failing tests: none known for this bead.
- Relevant metrics: managed dashboard evidence from caco-web duty cycle 0047 showed web UI version `v1.2.567` while current main/macOS apps were around `v1.2.570`.
- Context: a responsive managed dashboard could still serve old frontend assets after an update, causing the Summaries route to issue the old unscoped `limit=200` request and end in a handled daemon-proxy timeout rather than making stale asset drift visible.

## After state

- Failing tests: none observed in the rerun validation.
- Relevant metrics: focused status/lifecycle tests and `cargo test-small` passed after rebasing onto current `origin/main`.
- Context: `caco web status` now exposes served version, expected package version, and a `version_drift` flag, while PID-only lifecycle convergence can detect a responsive but stale `caco-web` via `/health.version` and restart it.

## Diff summary

- Commits: `58e713fc6` (`bd-f74047: detect stale managed caco-web assets`), plus the recorded summary commit
- Files touched: `crates/caco-sidecar/src/lifecycle.rs`, `crates/caco-cli/src/lib.rs`, `README.md`, `AGENTS.md`, `SPEC.md`
- Tests: added/updated focused Rust tests for web-status version drift and caco-web lifecycle stale-version detection; no tests removed.
- Behavioural delta: responsive PID-only `caco-web` processes are no longer assumed healthy when their served `/health.version` differs from the current package version, and operators can see the same drift directly in `caco web status` output.

## Operator-takeaway

A long-running managed dashboard can now self-identify as stale after an update: status output shows the drift, and the lifecycle watchdog has a safe caco-web-specific reason to respawn stale assets while preserving the prior external-port PID-only service contract.
