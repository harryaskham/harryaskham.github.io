# Session summary — bd-6c1c5d caco hello-world local/cluster swap

## Goal
Fix `caco hello-world --json` labelling Tailscale IP as 'local' and loopback as 'cluster'.

## Bead(s)
- `bd-6c1c5d` — daemon listener local/cluster swap in hello-world JSON

## Before state
- runtime.listeners.daemon.local = `<bind_host>:<cluster_port>` (peer-reachable)
- runtime.listeners.daemon.cluster = `127.0.0.1:<cluster_port>` (loopback)
- Bootstrap consumers routed traffic exactly inverted.

## After state
- daemon.local = `127.0.0.1:<api_port>` (loopback, co-located only).
- daemon.cluster = `<bind_host>:<public_cluster_port>` (mesh-reachable).
- New `resolve_static_local_api_port` helper sources the API port; existing `resolve_static_cluster_contract` preserved (daemon-serve binder still needs cluster bind port).
- 2 new tests pin both helpers.

## Diff summary
- `crates/caco-cli/src/lib.rs` (+86 / -5): new helper, dispatcher fix, contract doc comment, 2 tests.
- `cargo test-small`: 153 passing.

## Operator-takeaway
`caco hello-world --json` now reports local/cluster correctly. Bootstrap tooling that consumed the JSON to choose dial addresses is unblocked.
