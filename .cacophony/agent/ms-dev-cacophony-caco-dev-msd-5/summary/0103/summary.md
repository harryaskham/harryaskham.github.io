# Session summary — embedded caco-web on loopback (bd-06bcee)

## Goal

run_embedded (the in-process Android embedded daemon) served ONLY the loopback
local API. To give the Android app a web/dashboard, run_embedded should ALSO
bind caco-web on a loopback port. (Part 1 of bd-76cb45; ttyd deferred — separate
binary, not in the .so.)

## Bead

- `bd-06bcee` (P3 feature) — run_embedded binds caco-web on 127.0.0.1 loopback.
- Coordinated with msd-0's bd-503a29 (SSH-tunnel multi-port) on the shared port.

## After state

- run_embedded now binds caco-web on `127.0.0.1:DEFAULT_EMBEDDED_WEB_PORT`
  (11180, the canonical caco_config `default_web_port` / caco-web `WebConfig`
  default) alongside the local API. Fixed, NOT derived from the API port — so the
  Android embedded mode and msd-0's SSH-tunnel mode converge on the same client
  config `{127.0.0.1, DEFAULT_WEB_PORT}` with zero per-mode port arithmetic.
- caco-web `serve()` has no graceful-shutdown future, so it runs as a tokio task
  aborted when the local-API server's graceful shutdown (the wrapper's oneshot)
  completes — clean single-shutdown ownership preserved.
- run_embedded's signature is UNCHANGED (md2-0's wrapper unaffected).
- ttyd stays deferred (separate binary not in the .so).

## Diff summary

- Code commit: `bd-06bcee: run_embedded binds caco-web on loopback DEFAULT_WEB_PORT`.
  Final landed squash SHA from the reintegration receipt.
- File: `crates/caco-daemon/src/lib.rs` (const + `build_embedded_web_config`
  helper + run_embedded dual-server wiring + doc update + test).
- Test: `cargo test -p caco-daemon --lib build_embedded_web_config`.

## Operator-takeaway

The Android embedded daemon now serves caco-web on 127.0.0.1:11180, so the app's
WebView reaches the dashboard with the same `{127.0.0.1, DEFAULT_WEB_PORT}`
client config as the SSH-tunnel mode (no per-mode branching). ttyd is the
remaining deferred piece.
