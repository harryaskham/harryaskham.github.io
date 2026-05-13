# Session summary — cross-node choice visibility

## Goal

Fix `bd-5b1509`, where a choice presented on one node could appear only on that presenting node's operator surface. The goal was to make replicated `choice_presented` / `choice_resolved` feed events update the same durable operator-inbox choice state that `/api/v1/choices/*`, TUI snapshots, web, and companion surfaces read.

## Bead(s)

- `bd-5b1509` — Choices only appear on presenting node instead of all operator surfaces

## Before state

- Failing tests: no targeted regression covered materializing a remote choice feed event into the local `operator_inbox` table.
- Relevant metrics: live operator report showed choice `choice-019e1f1d-ea28-77d1-8fe0-897676bf0938` visible in the helsinki TUI but absent from other nodes.
- Context: choices were persisted locally as operator-inbox rows and full-state sync could later copy those rows, but remote nodes learning first through replicated feed/UI events did not immediately create/update a local choices read-model row.

## After state

- Failing tests: none observed in targeted validation.
- Relevant metrics: two new `bd_5b1509` unit tests prove that a replicated `ChoicePresented` feed event creates an active local inbox choice row and that a replicated `ChoiceResolved` feed event transitions a materialized choice to `resolved` with resolution data.
- Context: full-state `recent_events` merge and snapshot history materialization now call the shared choice-feed materializer, so remote choice APIs and operator surfaces can see active choices before waiting for the slower `operator_inbox` family pass.

## Diff summary

- Code/content commits: `991906054` (`bd-5b1509: materialize replicated choices`)
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched: `crates/caco-daemon/src/operator_inbox.rs`; `crates/caco-daemon/src/replication.rs`; `SPEC.md`; `.cacophony/agent/winmini-cacophony-caco-dev-wmi-1/summary/pending/summary.md`
- Tests: +2 / -0 / flipped 0
- Validation: queued `caco test run --project cacophony --wait --command "cargo test -p caco-daemon --lib bd_5b1509 -- --nocapture" --cwd "$PWD"` passed after rebase as `tj-719e01cb`; `git diff --check origin/main..HEAD`; source assertions for `bd-5b1509` code and SPEC text.
- Behavioural delta: when a node receives replicated choice feed events, it now updates the durable choice/inbox read model used by local APIs and frontends, instead of relying solely on transient SSE or delayed full-state operator-inbox replication.

## Operator-takeaway

Choices should no longer be node-local in practice: presenting a choice on helsinki now has a durable feed-driven path for other nodes' operator surfaces to show and resolve the same active choice promptly.
