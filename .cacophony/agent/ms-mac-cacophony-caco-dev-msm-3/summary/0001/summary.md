# Session summary — bd-ab94cd: doctor recent-crashes excludes planned restarts

## Goal

Stop the doctor's `daemon recent crashes` sensor from flagging planned
SIGTERM/restart cycles (operator-driven `caco restart`, auto-update
version bumps) as crashes. During the v1.2.491→1.2.514 rollout window
the sensor was firing constantly even though every "crash" was a clean
shutdown.

## Bead(s)

- `bd-ab94cd` — doctor 'daemon recent crashes' sensor counts planned SIGTERM restarts as crashes

## Before state

- `daemon stopped: restart` followed by a fresh `daemon started` was
  recorded only as a `start` row in `startup-history.jsonl` — the doctor
  had no way to tell it apart from a real crash.
- During the eight v1.2.491→514 rollout cycles today the sensor reported
  `2 starts in last 15min` warning every pass, contributing to alert
  fatigue around the genuine helsinki blip cascade.
- Tests: `classify_recent_startups_warns_on_repeated_starts`,
  `classify_recent_startups_errors_on_boot_loop` (both still pass).

## After state

- `daemon stopped: <reason>` is now also written to
  `startup-history.jsonl` as a `kind=stop` row (with `reason`).
- `detect_boot_loop` and the CLI doctor `classify_recent_startups` both
  skip a `start` whose most recent prior entry is a clean stop
  (`restart` | `sigterm` | `shutdown` | `clean`) within a 120s pairing
  window.
- Legacy entries lacking `kind`/`reason` are treated as `start` so
  pre-rollout history continues to count.
- Unit tests added:
  - `crash_log::detect_boot_loop_excludes_planned_restart_cycles`
  - `crash_log::detect_boot_loop_counts_unpaired_starts`
  - `tests::classify_recent_startups_excludes_planned_restart_cycles`
  - `tests::classify_recent_startups_counts_unpaired_starts_as_crashes`
  - `tests::classify_recent_startups_excludes_all_clean_reasons`
- All `crash_log` and `classify_recent_startups` tests pass; `cargo
  clippy -p caco-daemon -p caco-cli --tests` clean.

## Diff summary

- Commit: `504baec6`
- Files touched:
  - `crates/caco-daemon/src/crash_log.rs` — `StartupEntry` extended
    (kind, reason, optional version), `record_shutdown` added,
    `count_unplanned_starts` helper, two new tests.
  - `crates/caco-daemon/src/lib.rs` — SIGTERM handler now calls
    `crash_log::record_shutdown` with the resolved reason.
  - `crates/caco-cli/src/lib.rs` — `classify_recent_startups` rewritten
    to apply the same pairing logic; three new tests.
- Tests: +5 (3 in caco-cli, 2 in caco-daemon).
- Behavioural delta: doctor `daemon recent crashes` no longer fires for
  planned restart cycles; genuine unpaired starts still warn/error at
  the existing thresholds (≥2 / ≥5 in 15min).

## Operator-takeaway

Doctor noise during rollout windows should drop sharply once this lands
and the next daemon restart writes its first paired `stop`/`start`
entry. The fix is conservative: legacy histories and unpaired starts
still count, so the sensor still surfaces real boot-loop incidents.
This was orthogonal to the parallel bead-store regression
(2774→113 reconcile drop, owned exclusively by helsinki caco-ctrl);
this session intentionally avoided any bead-store interaction during
that incident.
