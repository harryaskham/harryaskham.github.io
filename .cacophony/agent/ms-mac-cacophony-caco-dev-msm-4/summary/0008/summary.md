# Session summary — bd-45a6cb: surface fork+exec early-exit in start_sidecars_as_processes

## Goal

Discovered while writing test coverage under bd-2f3840. On Linux,
`std::process::Command::spawn()` uses fork+exec — the exec failure
happens in the forked child, not the parent — so the parent's
`spawn()` returns Ok with a Child handle even when the launcher
binary is missing or not executable. The probe loop then times out
(the child died), but the function unconditionally pushed the
service name into `started` and returned Ok. This bead's whole
purpose was the literal pin-test-flip: the bd-2f3840 author left
an explicit pinning test
(`start_sidecars_as_processes_currently_returns_ok_for_bogus_launcher`)
that documented the bug and asked the fixer to flip its assertion.

## Bead(s)

- **bd-45a6cb** (P3 bug, owned).
- bd-2f3840 (sibling — left the pin test in place).

## Before state

- `LifecycleManager::start_sidecars_as_processes` spawned the
  child, wrote a PID file, ran a 10-iteration probe loop with
  ~1.9s total backoff, and unconditionally pushed the service name
  into `started` regardless of whether the probe ever succeeded.
- A bogus launcher path therefore returned `Ok(["caco-daemon"])`
  with a stale PID file left on disk.
- Downstream liveness checks would notice the dead sidecar after
  several converge cycles, but by then the operator was looking at
  a cascade of 'sidecar unreachable' warnings rather than the
  true root cause.

## After state

- The spawned `child` is now bound `mut`. Each probe iteration
  calls `child.try_wait()`. If the child has exited before the
  sidecar becomes reachable, `early_exit = Some(status)` and the
  probe loop breaks.
- After the loop, if `early_exit.is_some()`:
  * the PID file we wrote is removed (no stale PID left behind);
  * the function returns `Err` with a structured message
    containing the failing service name, the exit status, the
    `bd-45a6cb` breadcrumb, and the tail of the sidecar log
    (via the existing `tail_log` helper).
- The bd-2f3840 pin test is flipped to assert the new contract:
  `expect_err`, error must name the service and mention either
  'exited immediately' or 'bd-45a6cb', and the PID file must NOT
  be left behind.
- `write_capture_script` test helper now `sleep 5`s after writing
  args (instead of `exit 0`) so the positive-path tests stay
  alive past the probe loop and continue to be treated as a
  successful spawn — their assertions on captured args/node are
  unchanged.
- 6/6 `start_sidecars_as_processes_*` tests pass.
- Two pre-existing test failures (`resolve_pid_exe_returns_self`,
  `stale_detection_true_when_binary_path_mismatches`) reproduce on
  pristine main and are unrelated (they read `/proc` which
  doesn't exist on macOS); confirmed via `git stash` baseline.
- Clippy clean.

## Diff summary

- Commit `b45338a1`: bd-45a6cb: surface fork+exec early-exit in
  start_sidecars_as_processes.
- Files touched:
  - `crates/caco-sidecar/src/lifecycle.rs` (+66 / -23):
    try_wait wiring, early-exit Err path, pin-test flip,
    capture-script `sleep 5`.
- Tests: 0 net-new tests; 1 pin-test flipped from "Ok pin" to
  "Err contract".
- Behavioural delta: a bogus launcher binary now surfaces as a
  structured Err at `start_sidecars_as_processes` time instead of
  bubbling up as a cascade of confused liveness warnings several
  converge cycles later. Stale PID files are no longer left
  behind on this failure path.

## Operator-takeaway

The "deploy-time launcher regressions are invisible at the
lifecycle layer" footgun is closed. Downstream effects:

- Nix profile rollback / packaging mistake / accidental rename
  of the caco binary now produces a clean, attributable error
  the moment converge tries to spin up sidecars, with the
  sidecar log tail folded into the error so the operator can
  see *why* the child died (binary not found, dynamic linker
  mismatch, panic on init, etc.).
- No more stale PID files on this failure path → less spurious
  is_pid_alive false-positives during recovery.
- The bd-2f3840-style "leave an explicit pin test for the next
  author to flip" pattern is well worth continuing for known
  bugs we don't have time to fix in the discovering session.
