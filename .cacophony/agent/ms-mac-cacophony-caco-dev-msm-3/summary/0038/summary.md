# Session summary — bd-65813b slice 1: LAST CRASH banner on daemon respawn

## Goal

Daemon deaths at ~20:47 and ~21:47 left NO actionable trace: stderr
pointed at `daemon-crash.log (deleted)`, sidecar.log was last touched
3 days prior. The reproducible mitigation from criterion 4 is to echo
the previous crash log into the new daemon log as a `LAST CRASH:`
banner on every respawn — every restart self-diagnosing.

## Bead(s)

- `bd-65813b` — Daemon dies silently on startup — crash log is
  unlinked while fd held open, losing panic/OOM/config-parse output

## Before state

- `start_service` opened `daemon_crash_log` with `create+append`,
  passed the fd to the spawned daemon as stderr, and never looked at
  the previous content. Any panic/OOM that landed in the crash log
  before this respawn was invisible to the operator until they ran
  `caco crash-log` (and most don't).
- The convergence loop (line ~2415) had partial mitigation: it logged
  a one-line `last_crash_line=...` hint alongside the
  `service ... exited` event when re-converging, but only the LAST
  non-empty line, not the actionable backtrace tail.

## After state (criterion 4)

- New `pub fn emit_last_crash_banner_if_present(service_log,
  crash_log, service_name, max_lines)` in `caco-sidecar::lifecycle`.
- `start_service` calls it for `caco-daemon` only, just before
  opening the stderr fd, so the new daemon's stderr appends after
  the truncation point.
- Banner format:
  ```
  [TS] [supervisor] LAST CRASH (caco-daemon): previous caco-daemon
  stderr (N lines, showing last K) follows. See bd-65813b.
  ----- BEGIN LAST CRASH -----
  ...tail (last 40 lines)...
  ----- END LAST CRASH -----
  ```
- After echoing, the crash log is **truncated in place** via
  `OpenOptions::write(true).open(...).set_len(0)`, NOT
  `remove_file` + recreate. This preserves the inode so the bd-a95d90
  invariant (sidecar opens the crash log once and any inherited
  stderr fd continues working) is intact. Subsequent
  non-crashing restarts therefore find the crash log empty and
  emit no banner — no infinite-banner repetition.

### Tests (`crates/caco-sidecar/src/lifecycle.rs`)

- `emit_last_crash_banner_writes_banner_and_truncates`: seed a
  realistic panic + backtrace, assert (a) banner appears in service
  log with `LAST CRASH (caco-daemon)`, `BEGIN/END LAST CRASH`,
  `bd-65813b` references and the panic content, (b) crash log is
  truncated to 0 bytes, (c) crash log inode is unchanged across the
  truncation (the bd-a95d90 fd-stability invariant).
- `emit_last_crash_banner_noop_when_crash_log_missing`: missing
  crash log must not even create the service log.
- `emit_last_crash_banner_noop_when_crash_log_empty`: whitespace-only
  crash log is treated as nothing-to-report.
- `emit_last_crash_banner_caps_at_max_lines`: 200-line crash log
  with `max_lines=5` must surface only the last 5 lines.
- All 4 pass; `cargo test-small` 57/57 pass; clippy clean.

## Diff summary

- 1 commit, 1 file (`crates/caco-sidecar/src/lifecycle.rs`).

## Deferred to follow-up beads (under bd-65813b)

- **Criterion 1 (don't unlink crash log)**: audited the workspace —
  no `remove_file` / `unlink` call targets `daemon_crash_log` in
  any caco-* crate, and `rotate_crash_log` already uses in-place
  truncation (with a long bd-65813b comment explaining why). The
  `(deleted)` tag observed in the field must come from external
  rotation (system logrotate, hand-cleanup, supervisord
  `stdout_logfile_maxbytes`, or a historical code path now removed).
  Recommend a follow-up bead to (a) document this invariant for
  packagers, (b) add a doctor sensor that fstats `/proc/self/fd/2`
  on Linux and warns if the inode is unlinked.
- **Criterion 2 (line-buffer stderr)**: requires either a workspace-
  wide audit of `eprintln!` (~1k+ call sites) and a guarantee
  switch to `BufWriter`, or a stdio-setup change in the daemon
  bootstrap. Defer to its own bead — too invasive for this slice.
- **Criterion 3 (structured `daemon exited code=N signal=X`
  supervision events)**: needs a supervisor-side `wait()` integration
  on the spawned `Child`. Currently `start_service` returns after
  the health probe and lets the OS reap the child elsewhere. Defer
  to a focused bead.

## Operator-takeaway

After rollout, the next time the daemon panics on startup, the very
next `caco log tail` (and the next operator who looks at
`daemon.log`) will show the prior panic + backtrace inline, with a
`LAST CRASH` banner pointing at bd-65813b. No more silent deaths.
