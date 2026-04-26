# Session summary — stale transition grace

## Goal

Handle the reopened `bd-2efa9c` evidence from the 12:20 router pass: three workers were marked stale consistently at the exact threshold edge (301-306 seconds on a 300 second threshold), so the prior list/direct convergence fix was not enough to prevent short-lived stale flaps during long foreground work.

## Bead(s)

- `bd-2efa9c` — Active workers remain marked stale with impossible no-tool-activity age

## Before state

- Failing tests: none in-tree; the failure was operational flapping in health surfaces.
- Relevant metrics: router reported `cqqcqwtn68gktqrr` stale at 306s, `j6kakbt361o1i5at` at 305s, and `o99l01hnn52l2547` at 301s, all on a 300s stale threshold.
- Context: these threshold-edge samples often cleared on the next bounded health pass, which made them poor lost-session signals and caused repeated reopen/close churn for the liveness tracker.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: added a 60s stale transition grace; a default-threshold worker at 306s stays `Running`, while one at 370s transitions to `Stalled` with an explicit `stale threshold 300s, grace 60s` diagnostic. Per-profile stale thresholds also include the grace in their diagnostic.
- Context: the effective stale timeout remains the operator-visible warning threshold, but live-tmux workers are only mutated to `stale` after the short grace window is exceeded.

## Diff summary

- Commits: `3d826ce24`
- Files touched: `crates/caco-daemon/src/agent/mod.rs`, `crates/caco-daemon/src/agent/lifecycle.rs`, `crates/caco-daemon/src/agent/tests.rs`, `SPEC.md`, `README.md`, `docs/daemon.html`
- Tests: `cargo fmt --all -- --check`, `cargo test -p caco-daemon 'reconcile_default_stale_timeout' -- --nocapture`, `cargo test -p caco-daemon reconcile_uses_per_agent_stale_timeout -- --nocapture`, `cargo test -p caco-daemon disk_refresh -- --nocapture`, and `cargo check -p caco-daemon --tests`.
- Behavioural delta: health passes should no longer mark workers stale at 301-306 seconds when the configured threshold is 300 seconds; genuinely quiet live-tmux workers still become stale after threshold plus grace.

## Operator-takeaway

The reopened evidence was a threshold-hysteresis problem rather than another list/direct mismatch: Cacophony now gives live workers a bounded 60 second grace before flipping to `stale`, reducing false alarms without hiding genuinely stuck sessions.
