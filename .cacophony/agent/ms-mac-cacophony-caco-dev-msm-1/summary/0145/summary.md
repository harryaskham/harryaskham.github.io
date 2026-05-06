# Session summary — persistent Pi bootstrap exit handling

## Goal

Fix the persistent Pi resume failure where `init.sh` could die during checkout bootstrap before writing readiness, especially around `eval "$(direnv export bash)"` after the ms-mac recovery. The goal was a narrow lifecycle/bootstrap change for `bd-57e88a`, without touching release, runner, or local ms-mac repair work.

## Bead(s)

- `bd-57e88a` — Persistent Pi resume can die during direnv bootstrap before readiness.
- Related incident context: `bd-dcafee` — ms-mac projects-empty/autowipe recovery.

## Before state

- Failing tests: no local failing regression existed for this exact shape.
- Relevant metrics: affected agents showed `resume_blocker=tmux_session_exited` with bootstrap tail ending at `checkout_bootstrap [1] eval "$(direnv export bash)"` followed by `FATAL: init.sh exited before readiness ... (exit 0)`.
- Context: `SPEC.md` requires checkout bootstrap to run before readiness, preserve environment mutations, and avoid modeling first-run env setup/direnv work as unexplained spawn failure when the session can otherwise converge.

## After state

- Failing tests: none observed in focused queued validation.
- Relevant metrics: queued test `tj-2199232b` passed the new `init_script_bootstrap_intercepts_clean_exit_before_readiness_bd_57e88a` regression; queued test `tj-c28a54c9` passed the existing real-exit-code bootstrap failure regression; queued `cargo check -p caco-daemon --lib` passed as `tj-f8710659`. After rebasing onto current `origin/main`, queued test `tj-56f31cd1` re-passed the focused regression, queued `cargo check -p caco-daemon --lib` passed as `tj-a9f46016`, and a second post-rebase focused run passed as `tj-31f61a7b`.
- Context: generated `init.sh` now intercepts bare `exit` only inside the checkout bootstrap window. A clean `exit 0` becomes a logged return so readiness can still be written, while non-zero exits continue to trip the existing ERR trap and abort startup with diagnostics.

## Diff summary

- Commits: `4b521866d3`, `9f405a274f`.
- Files touched: `crates/caco-daemon/src/agent/spawn.rs`, `crates/caco-daemon/src/agent/tests.rs`, `.cacophony/agent/ms-mac-cacophony-caco-dev-msm-1/summary/pending/summary.md`.
- Tests: +1 focused unit regression / -0 / flipped 0.
- Behavioural delta: checkout bootstrap still runs in the init shell so environment mutations persist, but a tool-generated or snippet-level bare `exit 0` can no longer kill persistent resume before readiness. The bootstrap log records the intercepted exit for operator diagnostics.

## Operator-takeaway

This narrows the ms-mac persistent Pi resume failure from an unexplained pre-readiness tmux death into a convergent bootstrap path when setup completed cleanly, while preserving hard failure behavior for non-zero bootstrap exits.
