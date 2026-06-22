# Session summary — bd-0eeea6 Slices 1+2: android-emulator-reaper detection core

## Goal
bd-0eeea6 (filed + claimed this session after I root-caused the ms-dev android-build
wedge): reap abandoned `.#android` emulator processes that wedge the node's gradle build
path. Slices 1+2 land the pure classification core + the `/proc` enumerator (read-only
detection); the live-owner determination and actual reaping land in later slices.

## What landed this reintegration (Slice 1 — pure core)
- `crates/caco-daemon/src/android_emulator_reaper.rs` (new):
  - Module doc capturing the root cause (an abandoned `.#android` emulator, or the
    emulator/qemu it orphans to PPID 1 when only its launcher is killed, holds the
    `.#android` devshell/gradle socket; subsequent builds wedge on
    `unix_stream_read_generic`; node-wide, not just the owner) and the CONSERVATISM
    CONTRACT (never reap an ACTIVE emulator; `has_live_owner` must fail safe to true).
  - `EmulatorProcCandidate` (pid, ppid, cmdline, age_secs).
  - `looks_like_android_emulator(cmdline)` — matches android-sdk-emulator / qemu-system /
    `.#android`+emulator; deliberately does NOT match the gradle build wrapper.
  - `is_reapable_abandoned_emulator(cand, min_age_secs, has_live_owner)` =
    looks_like_android_emulator && age >= min && !has_live_owner.
  - 3 unit tests: recognizes emulator cmdlines, ignores the gradle wrapper / non-emulator,
    reapable only when old + unowned + emulator (and NEVER when it has a live owner).
- `crates/caco-daemon/src/lib.rs`: `pub mod android_emulator_reaper;`.

## Validation
Queued `cargo test -p caco-daemon --lib android_emulator_reaper` (slice 1 tj-d80cc8d6,
slice 2 tj-0597fe03) PASSED, plus `cargo check --workspace --tests` (tj-c1115b0a) PASSED
(cargo / `.#caco-runtime`, independent of the wedged `.#android` gradle path). Pure
additive module: no match-over-extensible-enum, no cross-crate coupling. Daemon-Rust land
via the real cacophony-fast-tests cargo gate (not skip-hooks).

## Slice 2 (added) — read-only /proc enumerator
- `parse_proc_cmdline(&[u8])` (NUL-joined argv) + `parse_proc_stat_ppid(&str)` (ppid after
  the last `)`, comm-with-parens safe) — both PURE + unit-tested.
- `proc_start_epoch(pid)` (self-contained age via /proc stat starttime + /proc/stat btime,
  mirrors merge_queue::process_start_epoch) and `enumerate_android_emulator_candidates(now)`
  (#[cfg(linux)] read_dir("/proc") -> cmdline/ppid/age -> filter looks_like_android_emulator),
  read-only discovery only — no reaping yet.

## Next slices (fresh focus — the DANGER is slice 3)
(2) `/proc` enumerator (cmdline + age via the merge_queue `process_start_epoch` pattern);
(3) **live-owner determination — must NEVER reap an active QA emulator** (map emulator →
running test-job/agent; orphan PPID-1 + old + idle = reapable, else skip); (4) cadence
sweep + reap (mirror `reap_orphan_caco_web` SIGTERM→grace→SIGKILL, killing the full tree
not just the launcher); (5) agent-exit teardown; (6) `caco doctor`/`ops` diagnostic.

## Related infra beads filed this incident
bd-e3fed1 (active emulator vs gradle nix-daemon-socket contention — root-cause pinned via
lsof+ss-x), bd-ec9f5e (caco test cancel does not reap its `.#android` nix-develop wrapper).

## Diff
See the reintegration receipt for the final landed squash SHA.
