# Session summary 0009 — bd-3f729f daemon log rotation

## Goal

Land bd-3f729f: daemon plain-text log had no rotation policy at
all. Live `~/.cacophony/daemon/daemon.log` was 56MB with no cap;
on a forever-daemon this is silent disk pressure.

## Bead(s)

- `bd-3f729f` — primary, claimed and worked.

## Before state

`crates/caco-daemon/src/logging.rs` had no rotation: `append_line`
was a plain `OpenOptions::new().create(true).append(true).open()`
followed by `writeln!`. Live `~/.cacophony/daemon/daemon.log` on
helsinki was 56MB and growing without any cap; total daemon dir
6.2GiB. No env knob, no in-process rotation thread, no
logrotate config shipped.

## After state

`append_line` calls `maybe_rotate_log(path)` first. When the live
file exceeds `LOG_ROTATE_MAX_BYTES` (default 100MB), archives
shift up by one slot up to `LOG_ROTATE_KEEP` (default 7). Both
env-var-tunable. Worst-case daemon-log disk budget per node:
~800MB (live + 7 archives). Steady-state cost: one fs::metadata
call per log line.

## Investigation finding

Audited `crates/caco-daemon/src/logging.rs`: zero `rotat`,
`truncat`, `max_size`, `retention` matches. `append_line` was a
plain `OpenOptions::new().create(true).append(true).open()` +
`writeln!`. Nothing else (no logrotate config, no
daemon-internal rotation thread). Live tonight's helsinki:
56MB live log, 2.5MB crash log, 6.2GiB total under
`~/.cacophony/daemon/`.

## Diff summary

`crates/caco-daemon/src/logging.rs` (+128):

- `LOG_ROTATE_MAX_BYTES` const + `log_rotate_max_bytes()`
  reader: default 100MB, env-tunable for tests / operator knob.
- `LOG_ROTATE_KEEP_DEFAULT` const + `log_rotate_keep()` reader:
  default 7 archives kept alongside live (so worst-case ~800MB
  daemon log budget per node).
- `maybe_rotate_log(path)`: env-reading wrapper that delegates
  to `maybe_rotate_log_with(path, max_bytes, keep)`.
- `maybe_rotate_log_with`: pure function, callable from tests
  without env-var contention. Returns early when file < max.
  When keep=0, truncates in place (test-friendly mode). Else
  drops `.{keep}` archive, shifts `.1..keep-1` up by one slot,
  moves the live file into the `.1` slot. Caller's next
  append_line recreates the live file.
- `append_line` now calls `maybe_rotate_log(path)` before the
  OpenOptions+writeln! pair. Steady-state cost: one fs::metadata
  call per log line; rotation cost amortised across 100MB of
  output.
- 3 unit tests:
  - `maybe_rotate_log_below_threshold_is_noop` (steady state)
  - `maybe_rotate_log_above_threshold_shifts_archives` (full
    shift incl. drop-of-oldest semantics)
  - `maybe_rotate_log_keep_zero_truncates_in_place`

## Files touched

- `crates/caco-daemon/src/logging.rs` (+128)

## Operator-takeaway

`daemon.log` now self-bounds to ~100MB live + 7 rotated archives
~= 800MB total worst-case per daemon node. Existing oversized
logs will rotate on the next append after the rebuild lands.

Operators wanting tighter retention can set
`LOG_ROTATE_MAX_BYTES=10000000` (10MB) or `LOG_ROTATE_KEEP=3`
(~400MB total) without a rebuild — both read fresh on every
append.

To grep across rotations: `cat ~/.cacophony/daemon/daemon.log{,.[1-7]}`
in append order; `tac` for newest-first time order needs the
files in reverse. (A `caco daemon log --since` that walks
archives is a follow-up if operators want it; today the existing
caco daemon log only reads the live file.)

## Validation

- `cargo test -p caco-daemon --lib logging::`: 27 / 27 PASS.
- `cargo test-small`: 212 + 109 + 747 + 295 + 18 + 2818 + 56
  PASS (note: jumped from 739 → 747 in caco-cli, 8 new tests
  from peer landings; everything green).
- `cargo clippy --workspace --all-targets -- -D warnings`: clean.

## Notes / follow-ups

- Apply same policy to `daemon-crash.log` (currently unbounded;
  but smaller blast radius — crash banners are short). Follow-up
  bead candidate.
- Optional: gzip rotated archives (would add a deps-on-flate
  dependency; defer until size warrants).
- Optional: time-based retention (drop archives > 30d). Size-
  based with keep=7 is sufficient for the chronic-growth case.
- Optional: emit a feed event on rotation so operators see "log
  rotated to .1" in the feed; trivial follow-up if asked.
- Initial test attempt used `set_var` + `Mutex` + drop-guard and
  still leaked the threshold into `follow_log_file_picks_up_new
  _lines_after_offset` (which calls `append_line` via the public
  path). Refactored to expose `maybe_rotate_log_with(path, max,
  keep)` so tests bypass env entirely. Pattern worth reusing for
  any "rotation/threshold" config-by-env unit-testing.
