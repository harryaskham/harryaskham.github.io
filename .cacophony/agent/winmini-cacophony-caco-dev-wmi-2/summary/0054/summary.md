# bd-2d3c6a tailnet daemon bind support

## Bead
- bd-2d3c6a — Support tailnet bind mode for daemon local ports while preserving cluster mTLS

## Changes
- Added `nodes[].services.caco-daemon.local_bind_host` with default loopback semantics and documented/validated `tailnet` as a bind alias.
- Resolved `tailnet` bind aliases to the node effective public/tailnet host for daemon local and cluster bind startup paths.
- Updated daemon status output and `/api/v1/node` listener diagnostics to separate local bearer-token auth from cluster mTLS auth and show local/cluster bind modes.
- Taught lifecycle service probing to use the configured local daemon bind host instead of assuming loopback for caco-daemon.
- Updated SPEC, README, AGENTS, and schema docs for the new local bind contract.

## Validation
- `cargo test -p caco-config daemon_local_bind_host_tailnet_validates_bd_2d3c6a --lib -- --test-threads=1`
- `cargo test -p caco-cli daemon_tailnet_bind_alias_resolves_for_local_and_cluster_bd_2d3c6a --lib -- --test-threads=1`
- `cargo test -p caco-daemon daemon_listener_bind_modes_report_tailnet_bd_2d3c6a --lib -- --test-threads=1`
- `cargo test -p caco-config config_schema_covers_all_toplevel_sections --lib -- --test-threads=1`
- `cargo check -p caco-config --lib`
- `cargo check -p caco-sidecar --lib`
- `cargo check -p caco-cli --lib`
- `cargo check -p caco-daemon --lib`

## Notes
- `scripts/rustfmt-changed.sh --check` still reports pre-existing rustfmt drift in large crate-root files (`caco-cli/src/lib.rs`, `caco-config/src/model.rs`, `caco-config/src/validate.rs`), so changed hunks were kept focused and `git diff --check` was clean.
