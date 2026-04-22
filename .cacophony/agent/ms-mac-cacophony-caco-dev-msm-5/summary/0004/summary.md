# Session summary — bd-4acdd7: surface lifecycle supervisor health in caco doctor

## Goal

Stop the cluster from getting silently un-respawnable: when the native
lifecycle supervisor (launchd / systemd / supervisord) is installed but
not loaded, caco-daemon will not auto-respawn after SIGTERM or crash.
Make `caco doctor` flag that state explicitly so the operator sees it
before the next daemon stop leaves the node unavailable.

## Bead(s)

- `bd-4acdd7` — ms-mac lifecycle supervisor 'not loaded' — daemon
  SIGTERM at 01:38 left no respawn, recovered only by manual caco up

## Before state

- `caco doctor` had checks for config, bearer token, certs, daemon,
  cluster listener, sidecars, beads-primary routing, peer
  reachability, and project checkout state — but no check for the
  native lifecycle supervisor.
- `caco_daemon::native_supervisor::detect_native_backend()` already
  reports `active_state = "not loaded"` for the launchd
  bootout-ed case, but no surface called it.
- Operator-observable consequence on ms-mac (2026-04-22 01:38): the
  daemon stopped, was not respawned, and went unnoticed until inbox
  calls started failing with EADDRINUSE-shaped errors.

## After state

- Failing tests: none from this change. Pre-existing
  `tests::doctor_help_available` stack overflow is the same family as
  bd-e4f3e3 (already in_progress on msm-1) — not filing duplicate.
- `dispatch_doctor` in `crates/caco-cli/src/lib.rs` gains a `lifecycle`
  area that consumes `detect_native_backend()` and emits:
  - **error** + recovery hint when `active_state` is `"not loaded"`
    / `"unloaded"` or `installed=false`
  - **warning** + recovery hint when loaded but not running
  - **ok** with `last_exit_code` in detail otherwise
  - **warning** when detection itself fails (test/CI hosts may
    legitimately have no native supervisor)
- New unit test `doctor_includes_lifecycle_supervisor_section`
  asserts the section is rendered.
- `cargo clippy -p caco-cli --tests` clean.

## Diff summary

- Commits: `059f6458`
- Files touched: `crates/caco-cli/src/lib.rs`
  (+110 lines: lifecycle area in `dispatch_doctor` + 1 unit test)
- Tests: +1, 0 removed, 0 flipped.
- Behavioural delta: every `caco doctor` invocation now reports the
  native supervisor's load/active state with a recovery hint when
  installed-but-not-loaded.

## Operator-takeaway

Acceptance #1 of bd-4acdd7 (caco doctor flags `active: not loaded`)
landed. The next time a launchd `gui/<uid>` scope drops or any
supervisor unit gets bootout-ed, `caco doctor` will surface it as a
**lifecycle: native supervisor [error]** check with a recovery hint
to run `caco up` — instead of the silent-untilthe-next-SIGTERM mode
this bead documented. The remaining acceptance items (idempotent
`caco service load` rebootstrap; gui/501 vs system/501 scope review)
are separate workstreams; this bead's first cut is the surfacing fix.
