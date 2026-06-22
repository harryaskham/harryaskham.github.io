# Session summary — bd-0eeea6 Slice 5: wire the reaper live (cadence sweep)

## Goal
bd-0eeea6 (reap abandoned `.#android` emulators that wedge the node gradle build path).
The reaper mechanism (slices 1-4: detection, live-owner check, full-tree reap) already
landed as a tested public API. This reintegration is Slice 5: making it LIVE — a
periodic daemon background sweep that actually invokes the reaper, so abandoned orphan
emulators are auto-remediated instead of silently wedging the node.

## What landed this reintegration (Slice 5)
- `crates/caco-daemon/src/lib.rs`:
  - New `spawn_background_task("android emulator reaper sweep", ...)` — `#[cfg(linux)]`,
    a 30-minute loop calling
    `android_emulator_reaper::reap_abandoned_android_emulators(DEFAULT_REAP_MIN_AGE_SECS, now)`
    and logging reaped pids via `crate::elog!`. Mirrors the existing "zombie child
    reaper" periodic-sweep pattern.
  - Added `"android emulator reaper sweep"` to `EMBEDDED_SKIPPED_BACKGROUND_TASKS`
    (host process management is not a local-only embedded-mode maintenance loop).
  - Safety: the sweep reaps ONLY 6h+ orphaned (ppid==1) emulators with no live owner;
    an active QA emulator (live owner, far younger than 6h) is never touched, and on
    non-android nodes the `/proc` walk finds nothing so the sweep is a no-op.

## Next slice
(6) a read-only `caco doctor`/`ops` diagnostic surfacing abandoned-emulator candidates
(via `enumerate_android_emulator_candidates`) before they wedge. Then bd-0eeea6 is
feature-complete (auto-remediation of the android-build wedge) and closes.

## Validation
Self-validated `cargo check -p caco-daemon` (tj-4a5494c4) PASSED — the reint gate is
echo-disabled during the merge-train rollout, so self-validation before landing is the
interim discipline (per controller guidance).

## Diff
See the reintegration receipt for the final landed squash SHA.
