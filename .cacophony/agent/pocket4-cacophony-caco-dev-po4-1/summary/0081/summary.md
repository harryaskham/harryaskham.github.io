# Session summary — Downgrade expected-unreachable stale doctor rows

## Goal

Make `caco doctor` stop treating stale-agent rows on nodes that are explicitly configured as expected-unreachable as actionable fleet-health failures, while preserving the affected agent IDs and raw stale evidence for operator review.

## Bead(s)

- `bd-234a2a` — [doctor] Downgrade expected-unreachable node stale-agent rows in doctor health
- `bd-b1304d` — ms-mac root disk usage rose to 85 percent with rapid free-space drop (operational evidence-only investigation closed before this code slice)

## Before state

- `caco doctor` already filtered `node_unreachable=true` stale-agent rows out of the actionable stuck-agent check.
- Expected-unreachable nodes were intentionally not tagged as `node_unreachable` in the agent summary, so their stale rows still reached the doctor actionable path and could become `error` if persistent agents were present.
- ms-mac disk incident triage showed root space had recovered to ~150G free and that active managed Cargo targets were the largest first-party reclaim candidates; no destructive cleanup was performed.

## After state

- `caco doctor` builds an expected-unreachable node set from config health expectations and partitions those stale rows into a separate `expected-unreachable stale accounting` info check.
- The normal `stuck agents` check reports `ok` when the only stale rows belong to expected-unreachable hosts, and raw agent IDs remain visible through the info detail sample.
- A caco-cli unit test now verifies that an otherwise blocked stale row hosted on an expected-unreachable node is not classified as actionable.

## Diff summary

- Code/content commits: `de93e3ccd`
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: +0 / -0 / flipped 0; extended existing caco-cli doctor helper test coverage
- Validation: `git diff --check`; queued `RUST_MIN_STACK=33554432 cargo test -p caco-cli --lib doctor_stuck_agent_helper_identifies_accounting_only_watch_bd_1f67e1` passed as job `tj-2cdcefe8` (after an earlier failing run exposed a missing test fixture `node` field and was fixed).
- Behavioural delta: expected-unreachable node stale-agent residue is now informational doctor evidence rather than fleet-health error/warning input.

## Operator-takeaway

Doctor output should now respect configured expected-unreachable maintenance/sleep nodes for stale-agent accounting: operators still see which agents are stale, but those rows no longer masquerade as actionable stuck workers requiring replacement.
