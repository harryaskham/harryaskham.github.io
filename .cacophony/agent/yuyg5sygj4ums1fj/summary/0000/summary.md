# Session summary — immutable direct reintegration

## Goal

Implement bd-9e4be4: make direct reintegration safe for immutable managed agent checkouts, move default profiles away from `direct,recorded`, preserve explicit recorded compatibility, and keep session summaries visible after direct summaries move to the default branch.

## Bead(s)

- `bd-9e4be4` — redesign direct reintegration around immutable checkouts and safe plain direct summary handling.

## Before state

Direct reintegration could mutate or reason from the live agent checkout, legacy profile guidance still defaulted several persistent agents to `direct,recorded`, and the summary reader primarily served legacy `cacophony-state` records. Fresh technical-writer reproductions showed recorded direct could strand code and summary state on divergent refs.

## After state

Direct reintegration now creates backup refs, prepares squash merges in an isolated integration checkout populated from the worker HEAD, refuses stale branches without auto-rebase, emits a receipt with `checkout_mutated=false`, and leaves checkout-mutating recovery to explicit `caco agent rebase`. Plain `direct` keeps `.cacophony/agent/<id>/summary/...` as repo files that land on main. The daemon summaries API now unions default-branch summaries with legacy `cacophony-state` summaries and prefers the default-branch copy for duplicate project/agent/index records.

## Diff summary

- Updated `crates/caco-daemon/src/reintegration.rs` for immutable direct attempts, backup refs, isolated integration clones, receipt rendering, no auto-rebase defaults, and focused regression tests.
- Updated `crates/caco-daemon/src/summary.rs` and summary API handlers so CLI/TUI/web/Android shared endpoints see both default-branch and legacy state-branch summaries.
- Updated `SPEC.md`, `README.md`, `AGENTS.md`, `docs/reintegration-policy.md`, and persistent profiles to make plain direct the default and recorded explicit-only.
- Validation so far: `cargo check -p caco-daemon --lib --tests`; focused direct, recorded-compat, stale-rejection, merge-queue-rejection, and summary-union unit tests.

## Operator-takeaway

Plain direct is now intended to be the safe default: worker checkouts are treated as immutable inputs, summaries should still be written and committed under `.cacophony/agent/<id>/summary/...`, and all summary surfaces should show the union of new mainline summaries plus older `cacophony-state` summaries.
