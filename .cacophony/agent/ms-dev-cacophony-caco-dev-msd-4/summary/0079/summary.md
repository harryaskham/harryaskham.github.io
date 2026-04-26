# Session summary — caco ps PID and uptime

## Goal

Make `caco ps` more useful for quick local process triage by showing process IDs and uptime where Cacophony can derive them, instead of forcing operators and agents to correlate PID files, process tables, and daemon state manually.

## Bead(s)

- `bd-5eebfe` — caco ps: show PIDs and uptimes

## Before state

- Failing tests: none at claim time.
- Relevant metrics: `caco ps` listed services, sidecars, and agents with kind/name/state/project/tmux/cwd, but service and sidecar rows did not expose PID or uptime even when PID files existed.
- Context: local PID files already existed for daemon-managed services, and agent list rows already carried enough creation metadata once exposed by the daemon agent-list projection.

## After state

- Failing tests: none in validation.
- Relevant metrics: `caco ps` JSON rows now include `pid` and `uptime_secs` when known; text output includes `PID` and `UPTIME` columns. A smoke run showed `caco-daemon` and `caco-daemon-sidecar` with live PIDs and minute-scale uptimes.
- Context: daemon agent list rows now include `created_at`, enabling future agent uptime display when the local API snapshot carries that field.

## Diff summary

- Commits: `18866e87f`
- Files touched: `crates/caco-cli/src/lib.rs`, `crates/caco-daemon/src/lib.rs`, `SPEC.md`, `README.md`, `AGENTS.md`, `docs/cli.html`
- Tests: +1 source-level `caco ps` regression for PID/UPTIME rendering; `cargo test -p caco-cli dispatch_ps_renders_pid_and_uptime_columns_bd_5eebfe --lib`; `cargo check -p caco-cli -p caco-daemon`; `cargo build -p caco`; `docs/validate-pages.sh`; `cargo test-small`.
- Behavioural delta: local process inventory now surfaces PID/uptime directly in both text and JSON where derivable.

## Operator-takeaway

`caco ps` now answers the first triage question — “which process is this and how long has it been up?” — without leaving the Cacophony CLI.
