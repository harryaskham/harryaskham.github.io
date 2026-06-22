# Session summary — bd-fd2c76 Slice 3 (daemon EPIPE-panic hardening)

## Bead
bd-fd2c76 (P3, batchable) — harden ~287 raw eprintln! in crates/caco-daemon/src/lib.rs
against the EPIPE-panic-under-load class (eprintln!/println! .expect()-panic on a
momentarily-broken daemon stderr pipe). Slice 1 (msd-3, lines 1-5200, 35 sites) +
Slice 2 (android-releaser, 5201-7200, 41 sites) already landed. This is Slice 3.

## Change (crates/caco-daemon/src/lib.rs)
- 41 eprintln! -> crate::elog! on lines 7201-8720 (per-request handler + background
  clusters), via the bead's proven sed recipe.
- crate::elog! (pty_stream.rs macro_rules! elog -> write_diagnostic_line) is an
  EPIPE-safe true drop-in: same format args, best-effort write (never panics).
- Pure 41:41 macro-name swap. No behavior change beyond not-panicking-on-broken-pipe.

## Validation
- Verified CLEAN: 41 removed lines all eprintln!(, 41 added all crate::elog!(, no
  prose/string changes (recipe verify empty), git diff --check whitespace clean.
- cargo check --workspace --tests queued (per ctrl daemon-Rust land discipline —
  catches path-disjoint combination breaks; -p caco-daemon alone insufficient).
- Rebased fresh onto current main before the swap.

## Remaining after Slice 3
- ~158 eprintln! sites (lib.rs ~8720-end) for the next slice-takers (same recipe).

## Diff summary
See the reintegration receipt for the landed squash SHA. One file:
crates/caco-daemon/src/lib.rs (41 eprintln! -> crate::elog! swaps, lines 7201-8720).
