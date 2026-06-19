# Session summary — caco-web supervised default bind 0.0.0.0 → 127.0.0.1

## Goal

Close the real (supervised) caco-web 0.0.0.0 exposure that bd-ddace5 missed. The
loopback-default fix (bd-ddace5) changed the server default + the `caco web` CLI
`--bind` fallback, but the persistent dashboard is launched by the lifecycle
supervisor with an explicit `--bind <NodeWebConfig.bind>`, and that config
default was still `0.0.0.0`. So the supervised dashboard still bound
all-interfaces by default. Change the config default to loopback, directly
serving Harry's "don't expose caco-web on 0.0.0.0 until auth is fixed" directive.

## Bead(s)

- `bd-7712b6` — caco-web SUPERVISED launch still binds 0.0.0.0 by default
  (NodeWebConfig.bind default; bd-ddace5 missed the config path)
- sibling of `bd-e5f22f` / epic `bd-764efa` (kept OPEN — auth work remains)

## Before state

- Failing tests: none. `NodeWebConfig.bind` defaulted to `0.0.0.0`
  (`default_web_bind`, crates/caco-config/src/model.rs); the supervisor passes
  it verbatim (`lifecycle.rs:1142 web.bind.clone()`). So a config-driven /
  supervised caco-web still bound all interfaces.
- caco-web is DOWN fleet-wide (operator mitigation), so no live exposure — but it
  would re-bind 0.0.0.0 on the next bring-up.

## After state

- Failing tests: none expected. `cargo check --workspace --tests` green (compiles
  all test code, including caco-sidecar). `cargo test -p caco-config --lib`
  green. No test asserts the web bind default or the generated supervised bind
  (verified across caco-config + caco-sidecar). The caco-sidecar lib test could
  not be run to completion due to repeated daemon-restart-recovery (ms-dev-2
  devbox flapping), but the change has no test dependency.
- `default_web_bind()` returns `127.0.0.1` (secure loopback default). The
  supervised dashboard now binds loopback by default; network/tailnet exposure is
  opt-in via an explicit `services.caco-web.bind` (+ tailnet resolution bd-65f6b0)
  and an `access_token` (bd-e5f22f auth). The daemon/cluster `bind_host` is
  untouched (mesh-safe — the 12100 mTLS listener stays reachable).

## Diff summary

- Code commit: pending final squash SHA from the reintegration receipt.
- Files: `crates/caco-config/src/model.rs` — `default_web_bind()` `0.0.0.0` →
  `127.0.0.1` + the `NodeWebConfig.bind` doc comment.
- Tests: +0 / -0 (single default-value change; no test depends on it).
- Behavioural delta: the supervised caco-web binds loopback by default instead of
  all interfaces.

## Embedded artefacts

None (one-value config default change).

## Operator-takeaway

bd-ddace5 only closed the manual-`caco web` exposure; the actual persistent
dashboard (launched by the supervisor with an explicit `--bind` from
`NodeWebConfig.bind`) still defaulted to 0.0.0.0. This closes that real path with
the secure loopback default, mesh-safe (daemon/cluster binds untouched), directly
implementing Harry's directive. The auth/refuse-to-start/tailnet work continues
under the parent bead.
