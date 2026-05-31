# Session summary — Cap queued cargo CARGO_BUILD_JOBS (bd-2d039d)

## Goal

A queued `cargo clippy --workspace` job drove ms-mac into a ~10-minute severe
CPU/memory contention window (load avg peaked 45.90/68.43/48.74) that amplified
the bd-8cb2cd daemon-backpressure incident. Make the build/test queue throttle
cargo parallelism itself — queue-owned, not per-command — so a single
`--workspace` clippy job cannot oversubscribe a shared build host even when its
submitted command omits `CARGO_BUILD_JOBS`.

## Bead(s)

- `bd-2d039d` — Throttle queued cargo clippy --workspace so it cannot
  oversubscribe the build host (P2 bug, build-queue/daemon).
- Cross-ref: `bd-8cb2cd` (recurring ms-mac backpressure; this addresses one
  identified amplifier, not the whole incident). Same cluster as my earlier
  landed `bd-81f3c8` (/api/v1/node liveness hardening).

## Before state

- Failing tests: none specific.
- `resolve_platform_job_launch` (the single queue choke point used by both
  test_queue.rs and build_queue.rs) wrapped commands verbatim. The per-command
  default `CARGO_BUILD_JOBS=2` only applies to jobs enqueued without an explicit
  command; an agent's explicit `caco test run --command "...cargo clippy
  --workspace"` ran with no CARGO_BUILD_JOBS, so rustc compiled the whole
  workspace dep graph at full host parallelism. `--jobs` alone does not bound
  clippy's dep-graph compile phase.

## After state

- Failing tests: none. caco-daemon builds + clippy clean (only a pre-existing
  unrelated `LifecycleOperationError` unused-import warning).
- Every queued cargo command is normalized through resolve_platform_job_launch
  to prepend a bounded `CARGO_BUILD_JOBS` (default 2) when the command omits one.
  Queue-owned: applies to all queued cargo test/build/clippy jobs on every
  platform; respects an explicit CARGO_BUILD_JOBS; leaves non-cargo commands
  unchanged; disableable per host via `CACO_QUEUED_CARGO_BUILD_JOBS=0`.
- Tests: 20/20 queued_job_env tests pass (+6 new bd-2d039d tests).

## Diff summary

- Code commit: e40e105e95 (final landed squash SHA from the reintegration receipt).
- Summary artefact commit: intentionally omitted.
- Files touched: crates/caco-daemon/src/queued_job_env.rs (new
  normalize_queued_cargo_command / effective_queued_cargo_build_jobs /
  command_is_cargo_invocation / command_sets_cargo_build_jobs helpers + wiring in
  resolve_platform_job_launch + 6 tests); AGENTS.md + README.md build-contention
  docs.
- Tests: +6.
- Behavioural delta: queued `cargo clippy --workspace` (and any queued cargo job)
  now runs with bounded codegen parallelism, preventing shared-host
  oversubscription. No change for commands that already set CARGO_BUILD_JOBS or
  for non-cargo jobs.

## Embedded artefacts

None. (Acceptance criterion asking for a controlled queued clippy run while
sampling load + daemon liveness requires ms-mac host-side observation; the
code-side throttle and its unit tests are landed here. Recommend the controller
or an ms-mac-host agent run that controlled verification post-deploy.)

## Operator-takeaway

The queue serialized clippy jobs but never bounded how *wide* a single
`--workspace` clippy compiles, so one job could still saturate every core on
ms-mac and tip the daemon into backpressure. The throttle now lives at the
universal queue choke point (one place, both queues, all platforms) and is
queue-owned + host-tunable rather than relying on each agent remembering to
prefix CARGO_BUILD_JOBS. One acceptance criterion (a live controlled-load
verification on ms-mac) still needs host-side observation post-deploy.
