# Session summary — bd-fc60ff slice 1: atomic write of issues.jsonl

## Goal

Eliminate the recurring git SIGBUS-in-libz-ng coredumps on
helsinki (~1 every 5–10 min) caused by the reconciler truncating
`.beads/issues.jsonl` in place while a concurrent `git add` had
the file mmap()ed via index_fd.

## Bead(s)

- `bd-fc60ff` — git SIGBUS in libz-ng adler32 during 'git add
  .beads/issues.jsonl' (slice 1 of 4 acceptance criteria)

## Before state

`flush_journal_to_disk` in `crates/caco-beads/src/store.rs:2755`
called `fs::write(&self.journal_path, &new_content)` which does
`open(O_TRUNC)` then writes — i.e. it truncates the inode in
place. Any concurrent `git add` with an mmap on that inode gets
a corrupt mapping mid-read; the kernel raises SIGBUS when the
mmap is read past the new EOF. zlib-ng's adler32 SIMD path is
where the read happens, hence the libz-ng stack trace in the
bead's repro evidence.

## After state

New `atomic_write(path, bytes)` helper writes a sibling tmp file
(`.<name>.tmp.<pid>.<seq>`), fsync()s it, then rename(2)s it
over the target. rename(2) is atomic on POSIX and preserves the
OLD inode for any in-flight mmap readers — their mapping stays
valid until they close the fd, while the path points at the NEW
inode for the next reader. No truncation of an open mmap is
possible. SIGBUS class eliminated at the reconciler write site.

## Diff summary

- `crates/caco-beads/src/store.rs` (+145):
  - New `atomic_write(path, bytes)` helper near other top-level
    file utilities. Tmp filename embeds PID + static AtomicU64
    counter so concurrent writers in the same process don't
    collide. Best-effort tmp cleanup on every error path.
  - `flush_journal_to_disk` swap: `fs::write(...)` →
    `atomic_write(...)`. Comment block at the call site cross-
    references bd-fc60ff and bd-cf99b7 so future readers
    understand why the truncate-in-place pattern is forbidden
    here.
  - 3 new tests:
    - `atomic_write_swaps_inode_preserving_in_flight_mmap_safety`
      (the load-bearing one — asserts inode CHANGES; same-inode
      would re-introduce the bug).
    - `atomic_write_leaves_no_tmp_files_on_success`.
    - `atomic_write_creates_target_when_absent`.
- `cargo test -p caco-beads --lib`: 242/242 pass (no regressions).

## Embedded artefacts

(none)

## Operator-takeaway

Slice 1 is the minimum-viable fix that eliminates the SIGBUS
class. The remaining 3 acceptance criteria are independent
hardening:
- #2: process-local flock around the reconcile-write-then-git-add
  sequence (defence-in-depth against multi-process reconcile races).
- #3: shorter-term mitigation: 'git hash-object -w --stdin <
  issues.jsonl' bypasses mmap entirely. Largely redundant once
  atomic_write lands but useful as a belt-and-braces fallback.
- #4: doctor sensor that surfaces 'git coredumps in
  cacophony.service in last 1h > 0' so future regressions are
  visible without manual journal grepping.

Bead unclaimed for follow-on slices.
