# Session summary — bd-0eeea6 Slice 3: emulator live-owner check

## Goal
bd-0eeea6 (reap abandoned `.#android` emulators that wedge the node gradle build path).
Slices 1+2 (detection core: classification + `/proc` enumerator) already landed. This
reintegration is Slice 3: the live-owner determination — the conservative safety check
that the actual reaping (later slice) will gate on, so an ACTIVE QA emulator is never
reaped.

## What landed this reintegration (Slice 3)
- `crates/caco-daemon/src/android_emulator_reaper.rs`: + `emulator_has_live_owner(cand)`
  = `cand.ppid != 1`. CONSERVATIVE + fail-safe-to-owned: an emulator is treated as
  OWNED (never reaped) unless it has been reparented to init (`ppid == 1`) — i.e. its
  launching wrapper/agent/test-job died, orphaning it (the canonical wedge case: the
  4-day orphan whose launcher SIGTERM left the qemu reparented to PID 1). Any live
  parent — or an unreadable ppid (0) — is treated as a live owner. The reaping decision
  additionally gates on a conservative age threshold via `is_reapable_abandoned_emulator`,
  so only a long-abandoned orphan is ever a reap candidate (an active QA emulator is
  young and has a live parent).
- 2 new tests: `live_owner_false_only_for_init_orphan` (orphan=no-owner, live/unknown
  parent=owned) + `full_reap_decision_combines_age_and_live_owner` (old+orphan=reapable;
  old+owned=NOT reapable; young+orphan=not yet).

## Next slices
(4) the actual reaping (mirror `reap_orphan_caco_web` SIGTERM->grace->SIGKILL, killing
the FULL tree incl orphaned PPID-1 children, only for `is_reapable_abandoned_emulator`
candidates); (5) agent-exit teardown; (6) a read-only `caco doctor`/`ops` diagnostic
(uses `enumerate_android_emulator_candidates`). Then bd-0eeea6 closes.

## Validation
Queued `cargo test -p caco-daemon --lib android_emulator_reaper` (tj-555ddd3b) PASSED.
Pure additive (a decision fn + tests). Daemon-Rust land via the real cacophony-fast-tests
cargo gate (not skip-hooks). Canonical mirror healthy (Harry refreshed it).

## Diff
See the reintegration receipt for the final landed squash SHA.
