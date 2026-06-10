# bd-396f9f — Daemon periodic sweep of abandoned direct-integration temp checkouts

## Bead
bd-396f9f (cleanup/daemon-resilience/disk/reintegration, P2; filer caco-ctrl). The daemon's own `caco-direct-integration-*` reintegration temp checkouts accumulate into disk pressure on busy reintegration nodes (ms-mac dropped / to 20G), currently requiring a manual operator sweep each tick.

## Root cause
Direct reintegration creates an isolated integration checkout as a `tempfile::TempDir` under `std::env::temp_dir()` (`prepare_isolated_integration_checkout`, prefix `caco-direct-integration-`). On normal success AND failure the `IntegrationCheckout`'s held `TempDir` Drops and cleans it. The age-based prune `prune_stale_direct_integration_tempdirs_from_active_temp_root` (bd-07e3f9, recurses into sibling `nix-shell.*` temp roots) runs ONLY at reintegration-create-time. So when a reintegration is **killed mid-flight** (e.g. the MCP/HTTP connection drops mid-gate and the daemon-side process is reaped — the same class I hit on bd-3dce60), the `TempDir` Drop is skipped and the abandoned dir is only reclaimed at the NEXT reintegration's create-time prune — never on nodes that have stopped reintegrating.

## Change
Added a periodic background sweep loop in caco-daemon (`crates/caco-daemon/src/lib.rs`), mirroring the existing cleanup loops (store retention, sqlite vacuum, dynamic-node lease, completed-checkout retention):
- `spawn_background_task("direct-integration tempdir sweep loop", ...)` — every 15 min, runs the existing tested age-based prune via `spawn_blocking` (fs work off the runtime): `reintegration::prune_stale_direct_integration_tempdirs_from_active_temp_root(&std::env::temp_dir(), DIRECT_INTEGRATION_TEMP_PRUNE_AGE)`.
- Made `prune_stale_direct_integration_tempdirs_from_active_temp_root` + `DIRECT_INTEGRATION_TEMP_PRUNE_AGE` (4h) `pub` so lib.rs can reuse them.
- Local fs maintenance: NOT added to `EMBEDDED_SKIPPED_BACKGROUND_TASKS`, so it keeps running in the iOS in-process embedded daemon.

## Why the existing 4h age (not a shorter one)
The prune is age-only (dir mtime vs `max_age`); the 4h margin is deliberately longer than any reintegration so an in-flight temp checkout (whose top-level dir mtime is frozen while the gate writes into deep `repo/.git`/`repo/target` subdirs) is NEVER deleted mid-flight. A shorter age would need reliable in-use / no-active-process detection for the directory (the bead's "no holder" predicate) — non-trivial + platform-specific for a dir vs a flock'd lock file — and is left as a deliberate follow-up. The periodic 4h sweep still bounds accumulation, reclaims abandoned dirs on idle/stopped-reintegration nodes, and removes the manual-sweep requirement (the bead's core ask), with zero risk to in-flight reintegrations.

## Scope
Daemon's OWN `caco-direct-integration-*` temp dirs only. `nix-shell.*` / `DD-ios`/`DD-watch` build-subprocess temp are out of scope per the bead.

## Validation (daemon test queue)
- `cargo test -p caco-daemon --lib bd_396f9f` (tj-8962d09d): PASSED — `direct_integration_tempdir_sweep_loop_registered_bd_396f9f` (deterministic source-introspection: the loop is registered, calls the tested prune entry point, and is NOT in the embedded skip set). The underlying prune is already covered by the bd-07e3f9 tests.
- `cargo clippy -p caco-daemon --lib` (tj-8c19c7d0): my files (reintegration.rs/lib.rs) clippy-clean. The 1 remaining warning is pre-existing in agent/lifecycle.rs:10401 (`was_non_terminal` never read), unrelated to this change and not gate-blocking.
- Changed regions rustfmt-clean; `git diff --check` clean.

## Diff
See the reintegration receipt for the landed squash SHA.
