# Session summary — lifecycle non-interference harness

## Goal

Add a focused regression test for the lifecycle supervisor/service convergence layer so daemon, standalone beads daemon, web dashboard, and TTS daemon ownership states cannot regress into duplicate spawns or unsafe port cleanup. This is a test-harness slice for `bd-734cbe`, not a behavioural rewrite of the lifecycle manager.

## Bead(s)

- `bd-734cbe` — Add lifecycle non-interference integration tests for daemon, beads, web, and TTS services

## Before state

- Failing tests: none known for this slice before implementation.
- Relevant metrics: existing lifecycle tests covered several individual incident shapes (`bd-ae1853`, `bd-794ba8`, `bd-d9ecec`, TTS PID-only ordering), but there was no single combined non-interference harness exercising daemon + caco-bd-daemon + caco-web + caco-tts-daemon ownership in one convergence pass.
- Context: validation initially hit host disk exhaustion (`No space left on device`) and then retryable daemon-restart recovery for broader queued lifecycle runs.

## After state

- Failing tests: none in focused validation. Broader `cargo test -p caco-sidecar lifecycle -- --test-threads=2` attempts (`tj-5aaa236d`, `tj-95544f75`) were retryable infrastructure outcomes (`daemon_restart_recovered`), not test failures.
- Relevant metrics: focused queued test passed as `tj-4d1ce49c` before adding the explicit daemon assertion and as `tj-8ff785b2` after the final rebase/tightening; related caco-web lifecycle subset passed as `tj-bb9d6124`. First-party `caco prune run --delete` freed about 33.6 GiB when disk exhaustion blocked compilation.
- Context: the new harness now asserts a healthy daemon listener is already-running, warming caco-bd-daemon and live caco-tts-daemon PIDs are not overlapped, daemon-owned caco-web port conflict reports an actionable `bd-794ba8` warning, and the protected daemon surrogate remains alive.

## Diff summary

- Commits: `3ea0389d62` (`bd-734cbe: add lifecycle non-interference test`)
- Files touched: `crates/caco-sidecar/src/lifecycle.rs`
- Tests: +1 Unix tokio regression test (`lifecycle_non_interference_keeps_services_single_owner_bd_734cbe`); no tests removed.
- Behavioural delta: no production code changed. The regression suite now codifies the invariant that lifecycle convergence must not spawn duplicate bd/web/TTS services or kill a daemon-owned dashboard port when existing ownership/warmup evidence is present.

## Operator-takeaway

This landed the requested non-interference coverage as a focused sidecar lifecycle regression: future lifecycle changes now have a single test that fails if daemon, beads, web, or TTS ownership starts overlapping or interfering again.
