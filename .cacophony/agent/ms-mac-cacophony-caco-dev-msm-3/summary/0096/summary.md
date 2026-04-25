# Session summary — provider-ready resume retries

## Goal

Fix `bd-cd32b2`, a repeated ms-mac failed-agent class where resume revived tmux/runtime but provider-ready handoff timed out, and agents with zero or exhausted generic retry budget were immediately marked terminal failed.

## Bead(s)

- `bd-cd32b2` — ms-mac failed-agent class: resume_provider_ready_handoff_failed (13 instances, retry budget exhausted)

## Before state

- Failing tests: none known for this exact retry-policy path.
- Relevant metrics: live affected agents reported `resume_blocker=provider_ready_timeout`, `retry_count=0`, and `last_error="retry budget exhausted (0 retries) ... no fresh provider-side ready signal arrived within 60s ..."`.
- Context: provider-ready handoff timeouts occur after tmux/runtime revive, so on loaded nodes they can be bootstrap races rather than terminal checkout/runtime defects.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: provider-ready timeout failures now get two bounded extension retries beyond the configured generic retry budget, or two fallback retries with 30s backoff when no generic retry policy is present.
- Context: other resume blockers keep the existing retry-policy semantics and still terminal-fail when their generic budget is exhausted.

## Diff summary

- Commits: `4fef20b7a`
- Files touched: `crates/caco-daemon/src/agent/lifecycle.rs`, `crates/caco-daemon/src/agent/tests.rs`
- Tests: added three unit tests for the retry-decision helper; ran `cargo test -p caco-daemon provider_ready_timeout_ -- --nocapture`, `cargo check -p caco-daemon --tests`, and `cargo fmt --all -- --check`.
- Behavioural delta: reconciler resume failures caused by `ProviderReadyTimeout` no longer become immediate terminal failures solely because the generic retry budget is zero/exhausted; they are retried a bounded number of times with loud logging.

## Operator-takeaway

The ms-mac provider-ready race should now self-heal a couple of times instead of creating another batch of failed agents when the node is under load, while preserving strict terminal behavior for unrelated resume blockers.
