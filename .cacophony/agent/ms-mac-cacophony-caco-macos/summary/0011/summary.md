# Session summary — macOS load telemetry semantics

## Goal

Clarify Cacophony's macOS load reporting so a hot-but-stable Mac is not presented to operators as a critical CPU overload solely because Darwin load averages are high. The session focused on relabelling load average as scheduler/run-queue pressure across daemon telemetry, CLI, TUI, web, and profile guidance while preserving existing compatibility fields.

## Bead(s)

- `bd-eb9999` — [macos] verify load reporting for hot-but-stable hosts

## Before state

- Failing tests: none known for this bead.
- Relevant metrics: `sysctl -n vm.loadavg` on the Mac showed very high load averages around `145 149 135` on a 14-core host, while Test/Canary sockets and core services were otherwise stable.
- Context: operator feedback clarified that macOS was running hot but stable, so load average alone should not be narrated as a critical incident.

## After state

- Failing tests: none from the focused validation set.
- Relevant metrics: daemon telemetry now carries platform and load-average source metadata; macOS/Darwin load pressure remains visible but is informational unless paired with symptoms.
- Context: CLI, TUI, web, SPEC, and profile guidance now distinguish load average from CPU utilisation and suppress load-only warning/critical styling for macOS.

## Diff summary

- Commits: `0348478d7eec`
- Files touched: `crates/caco-daemon/src/replication.rs`, `crates/caco-cli/src/lib.rs`, `crates/caco-cli/src/node_cmd.rs`, `crates/caco-tui/src/state/mod.rs`, `crates/caco-tui/src/state/tests.rs`, `crates/caco-tui/src/views/status.rs`, `crates/caco-tui/src/app.rs`, `crates/caco-web/static/app.js`, `crates/caco-web/src/tests.rs`, `SPEC.md`, `.cacophony/profiles/caco-doctor.md`, `.cacophony/profiles/caco-macos.md`.
- Tests: added focused daemon, TUI, and web assertions for macOS informational load semantics.
- Behavioural delta: macOS load average is now labelled as load pressure/load average rather than CPU load, and high macOS load alone no longer drives warning/critical presentation.

## Operator-takeaway

The important change is semantic: macOS can be hot but stable, so Cacophony now keeps high Darwin load averages visible without escalating them as an outage unless real symptoms appear.
