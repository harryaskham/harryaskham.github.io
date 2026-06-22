# Session summary — caco_daemon::run_embedded (Android embedded daemon entry)

## Goal

Operator (Harry) asked an ms-dev/ms-dev-2 Android agent to advance the Android
embedded daemon. I coordinated a clean split and took the Rust/daemon-lane
pieces: the in-process `caco_daemon::run_embedded` entry (this session) and the
flake `.so` cross-compile derivation (bd-6f2b92, blocked on md2-0's crate).

## Bead(s)

- `bd-2ad544` — caco_daemon::run_embedded: in-process embedded local-only daemon
  entry (this session's landed work)
- parent: `bd-c249b9` — Android embedded daemon (md2-1 app/UI half)
- pair: `bd-57d798` (md2-0: caco-daemon-ffi wrapper + JNI + Kotlin),
  `bd-6f2b92` (msd-5: flake aarch64-android .so derivation, blocked on bd-57d798)

## Before state

- The embedded daemon (`CACO_EMBEDDED`, bd-f0afad) only existed as runtime
  local-only gating; there was no in-process entry an app/JNI bridge could call
  to start/stop a daemon. caco-cli built `DaemonConfig` inline before `build_state`
  + `run`, and `run()` is not embedded-safe (spawns `serve_cluster` + PKI
  bootstrap + webhooks + heavy background tasks).

## After state

- `pub async fn caco_daemon::run_embedded(runtime_dir, port, bearer_token,
  config_path, shutdown: oneshot::Receiver<()>) -> Result<(), DaemonError>`:
  serves ONLY the loopback local API on `127.0.0.1:port` with the caller's
  bearer token and graceful shutdown, via the existing
  `synthesize_embedded_config_with_dual_hash()`. Skips cluster/web/PKI/background
  tasks. The embedding wrapper owns the tokio runtime + shutdown channel.
- `token::write_node_token` writes the app's authoritative token before
  `build_state`'s `ensure_token` reads it.
- `build_embedded_daemon_config` is a testable helper (loopback-only, skip
  credentials, dummy cluster addr).

## Diff summary

- Code/content commit: `bd-2ad544: add caco_daemon::run_embedded ...`. Final
  landed squash SHA from the receipt.
- Summary artefact commit: intentionally omitted (no self-reference).
- Files touched: `crates/caco-daemon/src/lib.rs` (run_embedded +
  build_embedded_daemon_config + 2 tests), `crates/caco-daemon/src/token.rs`
  (write_node_token).
- Tests: `build_embedded_daemon_config_is_loopback_only_bd_c249b9`,
  `write_node_token_overwrites_with_caller_token_bd_c249b9`. Validation:
  `cargo check -p caco-daemon` clean + both tests pass.
- Behavioural delta: a host app/JNI bridge can now run a local-only caco daemon
  in-process and stop it via a shutdown signal.

## Operator-takeaway

This is the Rust half of the Android embedded daemon, landed for md2-0's
caco-daemon-ffi JNI wrapper to call (signature locked with md2-0:
`oneshot::Receiver<()>` shutdown). The key design call: `run()` is NOT
embedded-safe, so run_embedded does a minimal local-only serve rather than
wrapping `run()`. Next: md2-0 lands the wrapper crate calling run_embedded, then
I land bd-6f2b92's flake `.so` cross-compile derivation against it.
