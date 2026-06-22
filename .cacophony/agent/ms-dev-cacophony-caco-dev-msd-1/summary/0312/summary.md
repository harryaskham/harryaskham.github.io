# Session summary — bd-2ac49d: reintegration gate routes through the test queue (CACO_BIN self-set)

## Goal
Fix the reintegration gate falling back to the unbounded INLINE `bash -c` path instead of the daemon
test queue (`caco test run --wait --gate`, bd-d981f9), which dropped gate checks off the queue's
host-protection (CARGO_BUILD_JOBS / RUST_TEST_THREADS caps, per-project serialization, saturation-
aware retryable timeouts) and let an inline gate oversubscribe the shared host (the 2026-06-15
bd-ede788 cascade tell: zero queue jobs during a ~40min gate run).

## Bead(s)
- bd-2ac49d (P3 bug, daemon/gate/reintegration). Distinct-assigned by ctrl (Harry's directive) to
  avoid claim-collision. Single-bead scope.

## Root cause
`run_one_gate_command` (reintegration_gate.rs) uses `caco test run` (queue) when `CACO_BIN` is set,
else inline `bash -c`. `run_reintegration_gate` runs in the DAEMON process, so it reads the daemon
process's `CACO_BIN`. The launcher/service-wrapper normally exports CACO_BIN, but direct `caco
daemon` invocations or env drift can leave it unset, silently dropping the daemon-side gate onto the
inline path. (No production code self-set CACO_BIN for the daemon process; the only set sites were
for child processes — agent spawn, audio, file_auto_sync.)

## After state
- New pure helper `daemon_caco_bin_fallback(existing, current_exe) -> Option<String>`: returns
  Some(current_exe path) ONLY when CACO_BIN is unset/empty and the executable is resolvable; None
  when already set (preserve launcher/config value) or no executable (leave unset).
- Call site in `dispatch_daemon_with_mode`, right after the `caco-daemon-extra-env` set_var loop and
  BEFORE the tokio runtime spawns worker threads (thread-safe set_var point): if CACO_BIN is unset,
  set it from `std::env::current_exe()` (which IS the caco binary).
- Effect: the daemon-side reintegration gate now reliably routes through the test queue across all
  start paths. Explicit inherited/config CACO_BIN still wins (fallback only fills the unset case).
  A genuine no-daemon CLI gate never runs daemon startup, so the inline portability fallback for
  minimal/non-cacophony envs is preserved (run_one_gate_command unchanged). Bonus: any other daemon
  CACO_BIN consumer (agent spawn / child services) also gets a resolved value when it was unset.
- 1 pure unit test (preserve-when-set / fill-when-unset / empty-as-unset / no-exe-leaves-unset).
  caco-cli compiles clean; test passes.

## Diff summary
- crates/caco-cli/src/lib.rs: daemon_caco_bin_fallback helper (before dispatch_daemon_with_mode);
  call site after the daemon_extra_env_pairs loop; 1 unit test.
(Final landed squash SHA: see the reintegration receipt.)

## Operator takeaway
The reintegration gate now reliably uses the daemon test queue (bounded CARGO_BUILD_JOBS /
RUST_TEST_THREADS + per-project serialization + saturation-aware timeouts) instead of the host-
oversubscribing inline `bash -c` path, because the daemon self-resolves CACO_BIN from its own
executable at startup when the launcher/env left it unset. No behavior change when CACO_BIN is
already set; the minimal/non-cacophony inline portability fallback is preserved.
