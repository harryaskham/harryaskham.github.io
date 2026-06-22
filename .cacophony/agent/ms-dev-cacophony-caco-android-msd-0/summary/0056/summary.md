# Session summary — bd-0eeea6 Slice 4: the actual emulator reaping

## Goal
bd-0eeea6 (reap abandoned `.#android` emulators that wedge the node gradle build path).
Detection (slices 1+2) and the live-owner safety check (slice 3) already landed. This
reintegration is Slice 4: the actual reaping — enumerate, conservatively select, and
SIGTERM→grace→SIGKILL the full process tree of abandoned orphan emulators, never an
active QA emulator.

## What landed this reintegration (Slice 4)
- `crates/caco-daemon/src/android_emulator_reaper.rs`:
  - `DEFAULT_REAP_MIN_AGE_SECS` = 6h (conservative: an active QA emulator is far younger).
  - `select_reapable_emulator_pids(candidates, min_age)` — PURE: filters to pids where
    `is_reapable_abandoned_emulator` (looks-like-emulator && age>=min && no-live-owner).
    Unit-tested (old orphan selected; old-but-owned, young-orphan, non-emulator rejected).
  - `descendant_pids(root)` — `/proc` pid->ppid BFS, so the reap kills the FULL tree
    (the bd-0eeea6 lesson: SIGTERMing only the launcher orphans the qemu children that
    keep holding the `.#android` socket).
  - `reap_emulator_tree(pid)` — async SIGTERM -> 3s grace -> SIGKILL survivors -> verify
    (mirrors `reap_orphan_caco_web`, extended to the tree).
  - `reap_abandoned_android_emulators(min_age, now)` — pub async orchestrator:
    enumerate -> select -> reap each -> return reaped root pids. NEVER reaps an emulator
    with a live owner or younger than min_age.

## Next slices
(5) agent-exit teardown (reap the emulator an agent launched on its stop/discard);
(6) a read-only `caco doctor`/`ops` diagnostic surfacing abandoned-emulator candidates
before they wedge. Then bd-0eeea6 is feature-complete (auto-remediation of the
android-build wedge) and closes.

## Validation
Queued `cargo test -p caco-daemon --lib android_emulator_reaper` (tj-7840668f) PASSED
(after a tail-position `unsafe`-block compile fix). Daemon-Rust land via the real
cacophony-fast-tests cargo gate (not skip-hooks). The reaping is conservative: only
6h+ old orphaned (ppid==1) emulators are ever touched.

## Diff
See the reintegration receipt for the final landed squash SHA.
