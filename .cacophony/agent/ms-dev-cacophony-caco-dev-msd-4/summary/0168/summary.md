# bd-85cb20 — caco-daemon clippy: collapsible_if in lifecycle.rs heartbeat preview

## Goal
Clear a NEW caco-daemon clippy broken-on-main (re-broke bd-0e1697's clean) so cargo clippy --workspace
-D warnings (the real reint gate's clippy step + gate-restore prereq bd-ff92cd) passes.

## Before
agent/lifecycle.rs:8008 'this if statement can be collapsed' (clippy::collapsible_if), introduced by
bd-253bce (heartbeat unread-preview project-scoping). Would fail the real gate's clippy --workspace.

## After
#[allow(clippy::collapsible_if)] on the `if count>0 { if !summary_lines.is_empty()` heartbeat-preview
guard. Chosen over a full collapse because the inner body is ~50 lines (a large dedent on a hot/contended
file) and lifecycle.rs already uses 5 targeted clippy allows (codebase convention). Validated: cargo clippy
-p caco-daemon --all-targets -- -D warnings green (bj-a5b8c252).

## Diff
- Code commit: 6bddeee082 (defer landed squash SHA to the reint receipt).
- crates/caco-daemon/src/agent/lifecycle.rs: +#[allow(clippy::collapsible_if)] + rationale comment.

## Context
Found while post-land-validating bd-dbb459 (slice #1 circuit-breaker). bd-dbb459's circuit-breaker
defense-in-depth closed at slice #1 (slices #2/#3 skipped — marginal + recovery-risk per the bd-82aa30
audit; real fix is the operator tailscaled-policy bd-c5914e). Gate-restore clippy cluster (bd-ff92cd).
