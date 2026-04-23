# Session summary — bd-e4dc16 snapshot rotate cron + doctor sensor

## Goal

Wire automatic snapshot rotation into the daemon and surface
pinned-snapshot count in caco doctor (bd-542d54 acceptance #5).

## Bead(s)

- `bd-e4dc16` (P3 task) — snapshot rotate cron + doctor sensor.

## Before state

- bd-542d54 shipped `caco_beads::snapshots::rotate` primitive +
  auto-pin on shrink-abort.
- No daemon cron called rotate → unpinned snapshots accumulate
  forever without manual operator action.
- No doctor sensor for pinned-count → operators had no glanceable
  view of accumulated forensic state.

## After state

- New background task 'beads snapshot rotate loop' walks every
  configured project hourly (after 120s startup delay) and calls
  rotate(DEFAULT_RETENTION_DAYS=7). INFO log on bytes freed,
  WARN on errors. Skips projects without .beads dir.
- New doctor sensor 'snapshot pinned-count' (area: state) reports
  total_pinned + total + per-project breakdown. Always status=ok
  (info-only).
- Test: doctor_includes_snapshot_pinned_count_sensor.

## Diff summary

- `crates/caco-daemon/src/lib.rs`: +60 — cron wiring.
- `crates/caco-cli/src/lib.rs`: +81 / -1 — sensor + test.
- cargo test-small green (2864 tests, +15); clippy clean.

## Operator-takeaway

Snapshots now self-prune on the hourly cron without operator
involvement. `caco doctor` shows per-project pinned-count so
operators can see when a postmortem investigation is keeping
forensic state alive (and unpin once complete).

## Implementation note

Had to use area='state' for the new sensor: the doctor text
renderer filters by a hardcoded area allowlist (config/auth/pki/
daemon/services/mesh/projects/state/storage/runner/runtime/ci/
errors/version) and 'beads' would have appeared in JSON only.
Filed mental note for a future bead: the area allowlist could
become an extensible registry to avoid this footgun.
