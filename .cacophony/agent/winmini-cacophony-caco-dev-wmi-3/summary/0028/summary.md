# Pending summary — bd-8d08cc

## Bead
- bd-8d08cc — Add Tailnet-only pki.bootstrap mode without Tailscale Funnel management.

## Changes
- Added optional `pki.bootstrap.exposure` config with supported values:
  - default/omitted: `funnel` (existing Tailscale Funnel management behavior)
  - `tailnet_direct`: authority serves the bootstrap HTTPS listener directly on `bind_port` and skips Funnel setup/health checks.
- Added config validation and schema docs for the new exposure field.
- Extended bootstrap authority runtime state with `exposure` so `/api/v1/node` / bootstrap diagnostics can distinguish disabled, Funnel-managed, and Tailnet-direct bootstrap modes.
- Updated daemon bootstrap startup so Tailnet-direct authorities still start the listener but do not call `tailscale funnel` or require Funnel health.
- Updated `caco cert serve` / daemon state construction to materialize the configured exposure mode.
- Updated `SPEC.md`, `README.md`, and `AGENTS.md` to document Tailnet-direct bootstrap behavior.

## Validation
- `cargo check -p caco-config --lib`
- `cargo check -p caco-daemon --lib`
- `cargo check -p caco-cli --lib`
- `cargo test -p caco-config pki_bootstrap --lib`
- `cargo test -p caco-daemon bootstrap_exposure --lib`
- `cargo test -p caco-daemon authority_starting_tailnet_direct_marks_funnel_not_responsible_bd_8d08cc --lib`
- `cargo test -p caco-cert resolve_bootstrap_token_from_env --lib`
- `cargo test -p caco-cli cert_pull_subcommand_is_discoverable --lib`
- `cargo clippy -p caco-config --lib -- -D warnings`
- `cargo clippy -p caco-daemon --lib -- -D warnings`
- `cargo clippy -p caco-cli --lib -- -D warnings`
- `git diff --check`
- `./scripts/rustfmt-changed.sh crates/caco-config/src/model.rs crates/caco-config/src/validate.rs crates/caco-daemon/src/bootstrap.rs crates/caco-daemon/src/lib.rs crates/caco-cli/src/lib.rs crates/caco-cert/src/bootstrap.rs` formatted safe changed files and intentionally skipped pre-existing non-rustfmt-clean large files (`caco-config/src/model.rs`, `caco-config/src/validate.rs`, `caco-cli/src/lib.rs`) to avoid unrelated churn.

## Notes
- Some focused caco-daemon/caco-cli test invocations exceeded shorter local timeouts during cold compile / package-lock contention, then passed after the compile cache warmed.
