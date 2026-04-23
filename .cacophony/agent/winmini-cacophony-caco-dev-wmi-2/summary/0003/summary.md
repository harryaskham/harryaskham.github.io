# Session summary — `caco service load` for idempotent supervisor reload (bd-4acdd7)

## Goal

Close the operational gap that bit ms-mac at 01:38 BST on 2026-04-22:
the launchd `com.cacophony.lifecycle` unit was installed but
unloaded (`active: ? not loaded`), so when caco-daemon got SIGTERM
nothing respawned it. Recovery required manual `caco up`. The bead
asked for two things:

1. `caco doctor` flags `active: not loaded` as an error/warning, not
   silent state.
2. A first-class `caco service load` / `caco up` path that
   re-bootstraps the launchd unit if installed-but-unloaded,
   idempotently.

## Bead(s)

- `bd-4acdd7` — `ms-mac lifecycle supervisor 'not loaded' — daemon
  SIGTERM at 01:38 left no respawn, recovered only by manual caco up`.

## Before state

- Doctor section 4b ("Native lifecycle supervisor (bd-4acdd7)") was
  already wired in `crates/caco-cli/src/lib.rs`: it surfaces `not
  loaded` as an `error` check with a hint pointing at `caco up`. So
  acceptance criterion #1 was already satisfied on main.
- Acceptance criterion #2 was NOT satisfied: `caco service` had
  start/stop/restart/status/show/logs but no `load`. On launchd,
  `service start` calls `launchctl kickstart`, which fails when the
  unit isn't in the domain. So the only recovery was `caco up`,
  which mixes lifecycle reload with full daemon convergence — not
  the targeted reload the bead asked for.

## After state

New `caco service load` subcommand that idempotently (re)loads the
native supervisor unit. Backend behaviour:

- **launchd**:
  1. `launchctl bootstrap gui/<uid> ~/Library/LaunchAgents/<unit>.plist`
  2. `launchctl kickstart -k gui/<uid>/<unit>`
  Step 1 is treated as success-equivalent if stderr says "service
  already loaded" / "Service is already loaded" / "already loaded",
  so re-running `service load` against an already-loaded unit is a
  no-op success.
- **systemd**: `systemctl --user daemon-reload` then
  `systemctl --user enable --now <unit>`. Both idempotent.
- **supervisord**: `supervisorctl reread` then
  `supervisorctl update <unit>`. Both idempotent.

Per-step results (command, exit code, stdout, stderr,
treated_as_success) surface in both text and `--json` output, so
operators and tests can see exactly what ran.

## Diff summary

- `crates/caco-cli/src/lib.rs`:
  - `SERVICE_SUBCOMMANDS`: register `load` leaf with help text.
  - Dispatch table: route `service load` to `dispatch_service_load`.
  - New `dispatch_service_load` (~150 LOC) — runs the
    backend-appropriate ordered command sequence, marks benign
    "already loaded" stderr as success, returns text or JSON.
  - Test `service_subcommand_help_includes_service`: assert `load`
    is in the help subcommands list.
- Tests: `cargo test -p caco-cli --lib
  service_subcommand_help_includes_service` passes.
- `cargo test-small`: 58 passed.
- `cargo clippy -p caco-cli --tests`: clean.

Behavioural delta: a new operator-/agent-safe verb that recovers
the exact failure mode the bead documented, without invoking full
`caco up` convergence.

## Operator-takeaway

When `caco doctor` shows `lifecycle  native supervisor   error
not loaded  launchd (com.cacophony.lifecycle)`, the right
single-purpose recovery is now `caco service load` (not `caco up`
or hand-typed `launchctl bootstrap`). It bootstraps the unit into
the gui/<uid> domain and kickstarts it in one call, and is safe
to invoke when already loaded — useful for idempotent recovery
loops in caco-doctor or convergence scripts.

Note: if this scenario keeps recurring (gui/<uid> scope dropping
on user logout / fast user switch), the longer-term fix is to
move the unit to `system/` scope in install-service. That's out
of scope for this bead — flag it in a follow-up if seen twice.
