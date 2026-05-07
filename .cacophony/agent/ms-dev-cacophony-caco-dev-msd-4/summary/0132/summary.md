# Session summary — caco status false-negative hardening

## Goal

Lock in the ms-mac restart-window reliability follow-up `bd-ee0a3b`: `caco status` was reporting daemon/beads unreachable after restarts even while service status, direct node API, and `caco bd status` showed the control plane was healthy.

## Bead(s)

- `bd-ee0a3b` — ms-mac daemon unreachable after repeated restart window

## Before state

- Log-monitor and caco-ctrl observed repeated `caco status --json` false negatives after ms-mac restart windows: `ok=false`, `daemon.reachable=false`, and/or `beads_host.reachable=false` with stale restart down-reasons.
- In the same windows, `caco service status --json` was healthy/running, `caco bd status --project cacophony --json` returned authoritative ms-mac with fresh/syncing beads, and direct `GET /api/v1/node` on `127.0.0.1:11100` returned 200.
- A later retry shape showed `daemon.reachable=true` but `beads_host.reachable=false` while `caco bd status` was healthy/fresh.

## After state

- Daemon liveness probing in `caco status` now probes `/api/v1/config/hash` and `/api/v1/node` concurrently, both authenticated and unauthenticated, so a slow first probe cannot consume the liveness budget while another cheap/direct daemon endpoint would answer.
- Advisory port-holder lookup is now independently bounded to 250ms, preventing a slow `lsof`/owner lookup from causing the status gather to miss its wall-clock deadline after daemon liveness succeeded.
- Remote in-process beads reachability now treats a live canonical `/api/v1/beads` `primary_view` as proof that the authoritative beads surface is reachable; the peer list remains advisory and may time out independently.

## Diff summary

- Commits: agent-branch code commit `eadde06e6d` plus this summary commit; final mainline squash SHA is assigned during reintegration.
- Files touched: `crates/caco-cli/src/lib.rs`, `.cacophony/agent/ms-dev-cacophony-caco-dev-msd-4/summary/pending/summary.md`
- Tests: +1 unit test for bounded port-holder lookup; existing status reachability regression tests still pass.
- Validation: `cargo fmt --all -- --check` passed; queued `tj-2b37605a` passed `cargo test -p caco-cli bd_ee0a3b --lib && cargo test -p caco-cli status_reachability_falls_back_when_authenticated_node_probe_is_slow_bd_8573ef --lib && cargo test -p caco-cli status_stale_pid_file_live_daemon_api_reports_running_bd_b18846 --lib`; queued `bj-a9e79158` passed `cargo build -p caco`; queued `tj-d096277c` proved the rebuilt binary returned `daemon.reachable=true`, `beads_host.reachable=true`, and `down_reason=null` under a bounded status run; post-rebase queued `tj-6b22ad4e` passed the focused status tests again.
- Behavioural delta: status should no longer turn a healthy direct node/beads API into a stale restart-window outage solely because one probe, peer summary, or advisory process-owner lookup lagged.

## Operator-takeaway

This slice makes `caco status` trust the direct control-plane evidence that was already healthy in the incident reports. If direct daemon/beads APIs answer, status should report them reachable even when advisory peer/listener diagnostics are slow or stale.
