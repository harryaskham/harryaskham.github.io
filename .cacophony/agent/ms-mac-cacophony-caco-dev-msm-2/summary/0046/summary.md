# Session summary — standalone beads daemon status and ms-mac config

## Goal

Implement urgent `bd-0584f5`: make the ms-mac beads primary use the standalone `caco-bd-daemon` service-object configuration, add the missing first-party status surface for that service, and make process/lifecycle separation visible through existing operator status surfaces.

## Bead(s)

- `bd-0584f5` — Split ms-mac beads primary into standalone caco-bd-daemon
- Related coordination: `bd-8fdb05` — Audit and fix ms-mac daemon startup warming latency
- Reflection follow-up: `bd-5aaf1b` — Split caco status JSON structure tests from live health assertions

## Before state

- Failing tests: none specific to the bead; the operator/controller evidence was live-process/config drift.
- Relevant metrics: `.cacophony/beads.yaml` used legacy `primary: [ms-mac]`, which keeps beads in-process in `caco-daemon`; `caco bd daemon` exposed `serve` and `restart` but no `status`; `caco status` did not expose beads service mode/ports/PID in `beads_host`.
- Context: web and TTS were already separate managed processes on ms-mac, but beads was still coupled to the main daemon. `bd-8fdb05` remains responsible for broader startup-warming latency and full-state sync timing.

## After state

- Failing tests: one initial queued validation command was malformed (`cargo test -p caco-cli bd_0584f5 bd_daemon`) and failed before running tests; corrected queued validations passed.
- Relevant metrics: validation passed with `caco config validate --project-config-dir "$PWD/.cacophony" --json`, queued `cargo test -p caco-cli bd_0584f5` as `tj-c97708c1`, queued `cargo test -p caco-cli status_command_produces_json_with_valid_config` as `tj-aa7d28b3`, queued `cargo build -p caco` as `bj-6319ca8e`, and `git diff --check`.
- Context: source-built `./target/debug/caco --config "$PWD/.cacophony/config.yaml" bd daemon status --json` reports `mode: "standalone"`, listener `ms-mac:11101/12101`, sidecar `11201/12201`, and stopped service state before rollout; source-built `caco status --json` exposes `beads_host.mode: "standalone"`, port `11101`, cluster port `12101`, and a `caco-bd-daemon` service row.

## Diff summary

- Commits: `e8a076e8e` (amended with this summary before reintegration)
- Files touched: `.cacophony/beads.yaml`, `crates/caco-cli/src/lib.rs`, `SPEC.md`, `README.md`, `AGENTS.md`, this summary
- Tests: +2 focused caco-cli tests for `caco bd daemon status` help/discovery and standalone listener JSON; updated status JSON structure test to assert `ok` is a live-health boolean rather than always true in isolated fixtures.
- Behavioural delta: ms-mac's configured beads primary is now a standalone service-object on local port `11101` / cluster port `12101`; `caco bd daemon status` gives an operator audit surface; `caco status` and `caco ps` can show standalone mode, PID/port, and the `caco-bd-daemon` lifecycle row once the landed config is active.

## Operator-takeaway

This lands the source/config half of the beads service separation: the next rollout should start showing `caco-bd-daemon` as an independent managed service instead of hidden in the main daemon. Broader daemon startup-warming latency remains deliberately scoped to `bd-8fdb05`.
