# Session summary — Bounded live-starting persistent agents

## Goal

Resolve the helsinki doctor finding where persistent agents could remain in `starting` indefinitely even though their tmux panes were alive and repeatedly warning rather than making readiness progress.

## Bead(s)

- `bd-c3305a` — [doctor] helsinki persistent agents stuck in starting with live tmux but no readiness

## Before state

- Failing tests: existing coverage verified that the first stale-live `Starting` pass preserved the tmux pane and added a warning, but there was no coverage for repeated stale-live passes.
- Relevant metrics: helsinki on `caco 1.2.580` still showed persistent agent rows such as `helsinki-cacophony-project-health` and `helsinki-gfx-replacer-gfxr-ctrl` in `state=starting` with nested bd-5e336a warnings.
- Context: timeout-to-failed churn had already been removed; the remaining gap was that repeated live-starting evidence had no bounded operator-facing state transition.

## After state

- Failing tests: none in focused validation.
- Relevant metrics: `cargo test -p caco-daemon stale_starting_with_live_tmux -- --nocapture` passed; `cargo check -p caco-daemon --tests` passed.
- Context: the first stale-live starting reconcile pass still preserves the live tmux pane with a bd-5e336a warning, while a subsequent pass with an existing bd-5e336a warning moves the managed agent to non-terminal `stale` with bd-c3305a operator-action detail.

## Diff summary

- Commits: `e489a9a2d`
- Files touched: `crates/caco-daemon/src/agent/lifecycle.rs`, `crates/caco-daemon/src/agent/tests.rs`
- Tests: added 1 regression test for repeated stale-live starting agents and preserved the existing first-warning test.
- Behavioural delta: repeated live-starting agents no longer accumulate nested startup warnings forever; they keep their tmux pane but move to a bounded non-terminal stale/operator-action state instead of failing.

## Operator-takeaway

This keeps Harry’s requested safety property — no timeout-based failure for a live pane — while preventing persistent agents from sitting in `starting` forever. Operators should now see a bounded stale state that asks for action rather than an endlessly nested startup warning.
