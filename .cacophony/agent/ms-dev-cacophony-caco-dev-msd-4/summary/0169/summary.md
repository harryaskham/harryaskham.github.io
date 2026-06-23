# bd-154692 — warm/persist sccache for reint-gate compiles (option c: larger ceiling)

## Goal
Stop the reintegration merge-gate compiling COLD (~44min). Root cause (ms-dev-2-ctrl/ms-dev-ctrl 2026-06-21):
concurrent caco-daemon worker validations + the gate share one SCCACHE_DIR and evict each other's objects;
cold gates saturate the test queue → widen the bd-0ffc21 staleness window → stale-reject churn.

## Before
flake.nix cacoRuntimeShellHook (shared by the `default` + `caco-runtime` shells, inherited by the queued
gate's `nix develop --command` wrapper) exported SCCACHE_DIR + RUSTC_WRAPPER + CARGO_INCREMENTAL=0 but never
SCCACHE_CACHE_SIZE → sccache used its 10G default → aggressive eviction under concurrent compiles.

## After
+ export SCCACHE_CACHE_SIZE="${SCCACHE_CACHE_SIZE:-30G}" in cacoRuntimeShellHook (env-overridable). A 30G
ceiling holds several concurrent workspace object sets so the gate stays warm. Single shared-hook edit →
both shells + the gate's nix develop inherit it. Complements bd-0ffc21 + bd-a54742 on the compile-SPEED
axis; indirectly shortens the bd-e69955 staleness drift window (shorter compile → shorter queue-wait).

## Validation
nix develop .#caco-runtime: SCCACHE_CACHE_SIZE=30G exported, wiring intact, sccache caching live (123 hits).
Flake evaluates clean. The 30G takes effect on the NEXT sccache server start (sccache reads it at startup;
shared server NOT force-restarted to avoid disrupting concurrent compiles). Warm-gate-under-concurrent-load
throughput is the needs-specialist-repro follow-up (the bead's own label).

## Diff
- Code commit: b4e9b3ad95 (defer landed squash SHA to the reint receipt).
- flake.nix: +SCCACHE_CACHE_SIZE export in cacoRuntimeShellHook.

## Follow-ups (noted, not in this slice)
- Optional: extend the bd-f49a71 caco doctor sccache-wiring sensor (caco-cli lib.rs, hot) to also assert
  SCCACHE_CACHE_SIZE so a future flake cleanup can't silently drop it.
- needs-specialist-repro: measure gate compile wall-time warm vs cold under concurrent worker validation.
