# Session summary — bd-274c2d test-health cycle 2026-04-22

## Goal

Permanent test-health cycle: run cargo test-small + clippy
workspace, report breakages and slow tests, fix what's mine.

## Bead(s)

- `bd-274c2d` — Permanent: continuous test suite health (P1 task)

## Before state

- Open ready queue had no claimable single-shot beads matching this
  agent's recent supervision/spawn focus.
- Last test-health cycle (msd-3 @ 1612bc58, 2026-04-22T02:09Z)
  reported cargo test-small 4141 PASS and three clippy fixes
  landed; no full-workspace test pass had been recorded since.
- msm-1 had broadcast a fresh observation that ~83/848 caco-cli
  tests fail under the now-runnable full suite, and a transport-
  error envelope test cluster was SIGABRTing the run — needing a
  Linux-side reproduction to confirm scope.
- Failing tests: same as After state (this is an observation cycle).

## Cycle results (Linux, helsinki dev env, agent msd-2 @ 600d04ca)

### cargo test-small
- **PASS**: 45 / 45 tests, 0 failed, ~2m34s wall (incl. compile from cold).
- This is the merge-queue contract preflight; no regression.

### cargo clippy --workspace --all-targets -- -D warnings
- **PASS**: clean, 1m34s.
- Confirms msd-3's prior cycle landed the strip_prefix /
  items_after_test_module / zombie_processes fixes cleanly.

### cargo test --workspace --lib --bins (full unit-test pass)
- **FAIL**: 84/848 caco-cli tests fail under `--test-threads=1`
  (763 pass).
- Reproduces msm-1's broadcast finding from this same cycle:
  shared mutable env state across tests in the same process. Failures
  are NOT thread-concurrency artefacts — they recur with serialised
  execution, so the contention is process-global env vars (CACO_CONFIG,
  CACO_NODE, CACOPHONY_*, USER, PULSE_SERVER, etc.) that one test
  mutates and another consumes.
- One real bug (not env): `tests::claude_requires_project_flag` —
  filed and being fixed by msd-3 (bd-51859d): the test does not
  isolate from a config-derived `default_project`, so it makes a 30s
  daemon call instead of failing fast. Triage bead being filed by
  msm-1 for the env-cluster.
- One blocking SIGABRT: caco-cli `tests::bd_send_request_*` /
  `bd_daemon_result_routes_transport_error_envelope` family stack-
  overflows on Linux too (msd-4 confirmed via cross-node DM — also
  reproduces on macOS per msm-1). Halts the suite before the env-
  cluster runs, so 84/848 is a lower bound on this checkout.

### Slow-test outliers
- None observed in `cargo test-small` (all under 200ms).
- Full `cargo test --workspace` total: ~3m wall. caco-cli alone is
  the slowest at 60–200s depending on threading; this is unsurprising
  given its sidecar-fallback config-load paths.

### Broken-on-main
- Pre-existing tmux set-environment failures in caco-daemon
  `persistent_recreate_relaunches_project_controller_replacement` /
  `running_persistent_agent_recreate_forces_destructive_relaunch` —
  msm-2 already filed a breakage bead. Local-tmux dependency, not a
  code regression.
- Pre-existing caco-cli env-isolation cluster — msm-1 filing triage
  bead this cycle.
- Pre-existing caco-cli SIGABRT in transport-error envelope path —
  msd-4 backing off bd-8e16e0 pending the underlying fix.

## After state

- No new breakages introduced.
- No fixes landed by this agent — coverage of the failing clusters
  is already claimed by msm-1, msd-3, and msd-4. Filing a duplicate
  fix would race those agents and risk double-mutation.
- Cycle observation: `cargo test-small` (the merge-queue gate)
  remains a tight, fast, hermetic preflight. The full
  `cargo test --workspace --lib --bins` is currently NOT a viable
  preflight gate due to env-cluster + SIGABRT — agents should keep
  using `cargo test-small` + targeted `-p <crate> --lib <pattern>`
  for change-local validation, as the merge-queue mixin already
  prescribes.

## Diff summary

- Commit: this summary only (no code changes).
- Behavioural delta: none. This is a pure observation cycle whose
  contribution is the recorded summary itself, the speak update on
  the project channel, and confirmation that the merge-queue gate
  is healthy.

## Operator-takeaway

The cluster is converging on the env-isolation cluster fix:
- msm-1 is filing the umbrella triage bead.
- msd-3 has a fix in flight for the most concrete real-bug member
  (`claude_requires_project_flag` — 30s daemon timeout because the
  test doesn't block a config-derived default_project).
- msd-4 is holding bd-8e16e0 until the SIGABRT family is unblocked.
- Operators can continue to rely on `cargo test-small` as the
  merge-queue gate; the 848-test full caco-cli surface is **not**
  currently a reliable signal due to shared-env contention and the
  SIGABRT block.

## Embedded artefacts

(none)
