# Pending summary — bd-ea5d61

## Bead
- bd-ea5d61 — Wire client_nodes identities into daemonless cert issue/pull/join flows.

## Changes
- Made configured `client_nodes` first-class certificate identities in the Rust PKI layer.
  - `caco_cert::issue` now resolves daemon-node or client-node cert identities and returns `identity_kind` diagnostics.
  - Cert status now includes client-node rows on authority/all-node views and labels rows with `identity_kind`.
  - Client-node SAN generation uses declared command-server host/bind when present and falls back to the stable client identity name.
- Extended bootstrap/authority surfaces for daemonless clients.
  - CSR `/v1/bootstrap/join` accepts configured `client_nodes` without registering them as dynamic daemon peers, preserving device-private-key CSR semantics.
  - `/v1/cert/pull` validates configured daemon/client identities and returns `identity_kind` in the pull payload.
- Extended CLI certificate flows.
  - `caco cert pull` now accepts optional `--node <client-node>` for configured daemonless client identity setup while preserving default current-node pull behavior.
  - Bootstrap authority URL/token resolution prefers per-client `client_nodes.<name>.bootstrap_url` and `bootstrap_token_file` when the target is a client identity.
  - Text status/pull output distinguishes daemon nodes from client nodes.
- Updated `SPEC.md`, `README.md`, and `AGENTS.md` to document client-node certificate identity behavior and diagnostics.

## Validation
- `cargo check -p caco-cert --lib`
- `cargo check -p caco-daemon --lib`
- `cargo check -p caco-cli --lib`
- `cargo test -p caco-cert --lib`
- `cargo test -p caco-config client_nodes_parse_and_diagnose_daemonless_clients_bd_950f67 --lib`
- `cargo test -p caco-cli cert_pull_subcommand_is_discoverable --lib`
- `cargo clippy -p caco-cert --lib -- -D warnings`
- `cargo clippy -p caco-cli --lib -- -D warnings`
- `cargo clippy -p caco-daemon --lib -- -D warnings`
- `git diff --check`
- `./scripts/rustfmt-changed.sh crates/caco-config/src/model.rs crates/caco-cli/src/lib.rs` intentionally skipped those two pre-existing non-rustfmt-clean HEAD files to avoid unrelated churn; changed Rust files that were safe to format were formatted.

## Notes
- Repeated broader `cargo test -p caco-cli cert_pull --lib` attempts timed out during cold test-binary compile before the narrower warmed `cert_pull_subcommand_is_discoverable` check passed.
