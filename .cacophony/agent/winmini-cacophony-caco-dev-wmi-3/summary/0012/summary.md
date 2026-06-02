# Session summary — bd-457120 dbus-daemon fork bomb on caco-vm micro-0

## Goal

Fix the P0 fork-bomb-class issue on the `caco-vm` `micro-0` microVM node,
where 35+ identical `dbus-daemon --syslog --fork --print-pid --print-address
--session` processes were accumulating in the process list, and document the
root cause plus a prevention strategy.

## Bead(s)

- `bd-457120` — Investigate dbus-daemon fork bomb on caco-vm micro-0 (P0 bug;
  labels caco-vm, critical, dbus, process-management)

## Before state

- Failing tests: none directly attributed; this is a runtime/process-management
  regression on the headless microVM node image.
- Context: 35+ orphaned `dbus-daemon --fork --session` processes on micro-0.
  The `cacophony-node` systemd service runs headless as `User=caco` with no
  session D-Bus, and `caco` audio paths shell out to `pactl`/`paplay`/`parec`.
  With `DBUS_SESSION_BUS_ADDRESS` unset, libdbus autolaunches a fresh private
  session `dbus-daemon` per invocation; these were never reaped or reused.

## After state

- Failing tests: none.
- Context: `cacophony-node` service environment now defines
  `DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/%U/bus`,
  `XDG_RUNTIME_DIR=/run/user/%U`, and `NO_AT_BRIDGE=1`, which suppresses
  libdbus/AT-SPI autolaunch so clients fail fast instead of forking a new
  bus. A `caco-microvm-health-check` binary is now the default boot-proof
  guest probe and fails (exit 70) if more than `CACO_MICROVM_MAX_DBUS_DAEMONS`
  (default 3) `dbus-daemon` processes are present, making any regression
  visible in boot-proof evidence. Investigation doc written.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted (no self-reference).
- Files touched:
  - `flake.nix` — `mkRichNode`: add `caco-microvm-health-check` binary;
    add dbus/AT-SPI env vars to the `cacophony-node` systemd service; export
    the same guards in the interactive rich-node shell. Inherited by base,
    `isolated`, and `firecracker` node variants.
  - `scripts/microvm-node-boot-proof.sh` — default guest probe is now
    `caco-microvm-health-check`.
  - `docs/microvm-node-rollout.md` / `.html` — document the new guest probe
    (sibling marker kept in sync).
  - `docs/investigations/bd-457120-dbus-fork-bomb-microvm.md` — new
    root-cause + prevention writeup.
- Tests: +0 / -0 / flipped 0 (no Rust source changes; flake + script + docs).
- Behavioural delta: headless microVM node services no longer autolaunch a
  private `dbus-daemon` per audio/desktop client invocation; boot proof now
  bounds the daemon count.

## Operator-takeaway

The "fork bomb" was D-Bus session-bus **autolaunch**, not a literal fork loop
in Cacophony code: every headless `pactl`/PulseAudio call with no
`DBUS_SESSION_BUS_ADDRESS` spawned a new orphaned `dbus-daemon`. Defining the
session-bus address (so libdbus fails fast) is the fix; the boot-proof health
check makes any future regression fail loudly instead of silently piling up
daemons. The general rule: headless system services that may touch
desktop/audio client libraries must set `DBUS_SESSION_BUS_ADDRESS` +
`NO_AT_BRIDGE=1`.
