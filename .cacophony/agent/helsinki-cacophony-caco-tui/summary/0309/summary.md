# Session summary — bounded no-gfx TUI runtime threads

## Goal

Investigate the Nix-on-Droid/Termux report that `caco tui --no-gfx` appeared as many repeated rows in htop on `sgu24`, and make one safe local improvement that reduces unnecessary TUI thread footprint and improves operator diagnostics without touching the remote process.

## Bead(s)

- `bd-95a978` — Investigate caco tui --no-gfx CPU/thread footprint on Nix-on-Droid sgu24

## Before state

- Failing tests: none known for the focused TUI runtime path.
- Relevant metrics: the `caco tui` dispatcher used `tokio::runtime::Runtime::new()`, which creates Tokio's default multi-thread worker runtime even for `--no-gfx`. On thread-aware mobile/proot tools such as htop, those worker threads can appear as repeated `caco tui --no-gfx` rows and be mistaken for duplicate TUI processes.
- Context: a bounded first-party read of `sgu24` from Helsinki still did not produce a useful remote status response before timeout, matching the bead's settling/mobile caveat; no remote process was killed or modified.

## After state

- Failing tests: the broad queued `RUST_MIN_STACK=33554432 cargo test -p caco-cli --lib` validation lane did not produce a trustworthy result. First run `tj-ccafe2ca` was a retryable `daemon_restart_recovered` infrastructure outcome; retry `tj-b996c342` hit the 3600s queue timeout with no durable cargo output. A draft follow-up `bd-5c6843` was filed to clarify/narrow that validation lane.
- Relevant metrics: `caco tui` now builds a dedicated current-thread Tokio runtime with `max_blocking_threads(4)` and logs `tui runtime initialized flavor=current_thread max_blocking_threads=4 ...`, so no-gfx TUI launches avoid the default worker pool and leave an explicit diagnostic for htop/thread-row interpretation.
- Context: operator-facing `docs/tui.html` now notes that thread-aware tools may still show helper threads as repeated rows and recommends checking PID/TID mode plus the TUI log runtime line before assuming duplicate processes.

## Diff summary

- Commits: final branch/reintegration commit to be assigned by `caco agent reintegrate`.
- Files touched: `crates/caco-cli/src/lib.rs`, `docs/tui.html`, `.cacophony/profiles/tui-animation-optimiser.md`.
- Tests: added `tui_runtime_tests_bd_95a978::dispatch_tui_uses_bounded_current_thread_runtime_bd_95a978`; no tests removed.
- Behavioural delta: interactive `caco tui` uses a current-thread async runtime with bounded blocking helpers instead of Tokio's default multi-thread worker pool. This should reduce repeated worker-thread rows for `caco tui --no-gfx` on constrained/mobile hosts while preserving async timers, IO, EventStream, SSE, and occasional blocking helper support.
- Validation: `docs/validate-pages.sh`; `./scripts/rustfmt-changed.sh`; `git diff --check`; queued `cargo check -p caco-cli` (`tj-d8cbaaab`); queued focused `RUST_MIN_STACK=33554432 cargo test -p caco-cli --lib tui_runtime_tests_bd_95a978` (`tj-58e49ebd`); queued `cargo clippy -p caco-cli --lib -- -D warnings` (`tj-59adf604`). Broad unfiltered caco-cli lib validation timed out as described above and is not treated as a code failure for this narrow slice.

## Operator-takeaway

The likely duplicate-row shape was addressed at the source by removing the default Tokio worker pool from interactive TUI launches and by documenting/logging how to interpret remaining helper threads; `sgu24` was observed only through first-party reads, with no remote cleanup or process kill.
