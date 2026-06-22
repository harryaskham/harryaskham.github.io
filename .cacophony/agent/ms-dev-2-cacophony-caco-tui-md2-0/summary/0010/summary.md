# Session summary — bd-edfdf8: crate-wide EPIPE regression guard (the capstone)

## Goal
Add a regression guard so the daemon EPIPE-hardening (bd-fd2c76 lib.rs + bd-c20d98 all modules, ~627 sites) cannot silently regress — a future raw eprintln!/eprint!/println! in non-test caco-daemon code now fails a test.

## Bead(s)
- `bd-edfdf8` — regression guard. Now crate-wide-capable (bd-c20d98 closed → 0 non-test print macros).
- Lineage: bd-bac84f/bd-a3467c (class), bd-fd2c76 (lib.rs), bd-c20d98 (all modules).

## Before state
- caco-daemon non-test code: 0 raw print macros (post bd-fd2c76 + bd-c20d98), but NOTHING prevented reintroduction.

## After state
- New integration test crates/caco-daemon/tests/epipe_guard.rs: walks src/**.rs at runtime (CARGO_MANIFEST_DIR), excludes *tests.rs files + each file's trailing `mod tests {` block (allowlisting pty_stream.rs's #[tokio::test] repro eprintln!), asserts no eprintln!(/eprint!(/println!( in non-test code. Passes now (0 offenders); fails on any future reintroduction, pointing the author at crate::elog!.
- Validation: queued cargo test -p caco-daemon --test epipe_guard (confirms it passes) + cargo check --workspace --tests (mandatory discipline).

## Diff summary
- Commit: pending final squash SHA from the reintegration receipt.
- 1 new file: crates/caco-daemon/tests/epipe_guard.rs (~75 lines). No src change (integration test = no hot-lib.rs edit, low rebase-conflict risk).
- Behavioural delta: none at runtime; adds a compile-time/test-time regression guard.

## Operator-takeaway
The capstone of the EPIPE-hardening arc (bd-bac84f -> bd-2ba5ed -> bd-fd2c76 -> bd-c20d98 -> bd-edfdf8). The whole caco-daemon diagnostic-print surface is EPIPE-safe AND now guarded against regression. crate::elog! (pty_stream::write_diagnostic_line) is the canonical non-panicking diagnostic path; the guard points violators at it. Integration-test approach keeps it self-contained and low-conflict.
