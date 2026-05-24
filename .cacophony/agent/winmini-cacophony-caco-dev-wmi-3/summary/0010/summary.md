# Session summary — bd-33c5f5 status backpressure beads probe

## Goal

Fix the `caco status` recurrence where a wedged local `/api/v1/node` status probe made the control plane look unavailable (`ok=false`, `daemon.reachable=false`, `api_backpressured=true`) even when the beads plane was still healthy via the canonical beads status endpoint.

## Bead(s)

- `bd-33c5f5` — caco status reports daemon unreachable/api_backpressured without maintenance while beads remain healthy

## Before state

- Status gather only attempted the canonical `/api/v1/beads` primary-view request inside the branch where `/api/v1/node` was already reachable.
- When `/api/v1/node` timed out/backpressured, `beads_primary_view` stayed absent and `beads_reachable` became false, so `status_control_plane_ok(false, false, true, false, false)` returned false even though external `caco bd status` checks could succeed.
- Recurrence evidence showed this exact split repeatedly on ms-mac: node/status read path backpressured, managed daemon PID alive, and beads status recovered/healthy.

## After state

- `dispatch_status` now independently probes `http://127.0.0.1:<port>/api/v1/beads` when the `/api/v1/node` status probe is not reachable.
- If that canonical beads endpoint returns a `primary_view`, the existing `beads_primary_view_from_beads_api` path marks beads reachability true, allowing status to report partial local API/read-path backpressure as degraded-but-usable rather than a hard control-plane outage.
- Focused queued validation passed in job `tj-20ae6e12`: `RUST_MIN_STACK=33554432 CARGO_BUILD_JOBS=2 cargo test -p caco-cli --lib status_probes_beads_when_node_probe_backpressured_bd_33c5f5 -- --test-threads=1`.

## Diff summary

- Code/content commits: `72df9fb99` (`bd-33c5f5: probe beads when status node probe backpressures`)
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: +1 source-level regression test / -0 / flipped 0
- Behavioural delta: `caco status` has an independent beads health proof path for `/api/v1/node` backpressure, aligning high-level status with the healthier `caco bd status` evidence observed by controllers.

## Operator-takeaway

The recurrence was a status-gather blind spot: the fallback status path treated missing node details as missing beads proof. The fix reuses the canonical beads status endpoint even when node details are wedged, so status can distinguish partial local API read-path degradation from a real core-control-plane outage.
