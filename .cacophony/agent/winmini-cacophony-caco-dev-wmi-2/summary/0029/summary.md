# broken-on-main wave #17 repair: is_git_dirty → git_dirty_kind test fixup

## Goal

Repair 2 test references to `is_git_dirty()` left behind by peer wmi-1's bd-215e3f rename to `git_dirty_kind()` (returns `(bool, bool)` instead of `bool`).

## Bead(s)

- (no claimed bead — pure broken-on-main repair)

## Before state

```
$ cargo clippy --workspace --all-targets -- -D warnings
error[E0425]: cannot find function `is_git_dirty` in this scope
     --> crates/caco-cli/src/lib.rs:75262:21
     --> crates/caco-cli/src/lib.rs:75267:22
```

## After state

Updated both test sites to use `let (modified, untracked) = git_dirty_kind(tmp.path()); modified || untracked` to preserve the original "is dirty (any kind)" assertion semantics.

Verification:
- `cargo clippy --workspace --all-targets -- -D warnings`: clean
- `cargo test-small`: 57/57 PASS

## Diff summary

1 file changed, +5 / −5:

- `crates/caco-cli/src/lib.rs`: 2 test sites converted from `is_git_dirty(p)` (bool) to `git_dirty_kind(p)` (tuple) with equivalent assertion

## Operator-takeaway

Wave #17 of this session — small renamer-leaves-test-references class. Caught by the merge-queue-gate-style local clippy run; no surprise. Quick fix, ~30 seconds of investigation.

Pattern pin: when peer renames a function, grep `--all-targets` (which clippy does, but `cargo build --release` doesn't) is the only way to catch test-only references. `bd-526670` (post-merge cargo build re-check on daemon side) would catch this if it ran on the merged tip.

Broken-on-main waves this session: 17 (cumulative). Most have been small one-or-two-fix reactive repairs.
