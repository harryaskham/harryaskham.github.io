# broken-on-main waves #15 + #16 repair (PersistentAgentDecl.goal field + percent_encode_query dead code)

## Goal

Repair two broken-on-main waves observed in the same repair cycle:
- **Wave #15**: peer wmi-1's bd-6b2af8 added `pub goal: Option<String>` to `PersistentAgentDecl`. 74 test-fixture sites in `crates/caco-daemon/src/persistent.rs` construct that struct without the new field (E0063 missing field).
- **Wave #16**: a `percent_encode_query` helper in `crates/caco-cli/src/lib.rs` became dead code after a recent refactor moved its only call site away (clippy `dead_code` denial).

## Bead(s)

- (no claimed bead — pure broken-on-main repair, bundled to keep the tree green)

## Before state

```
$ cargo build -p caco-daemon --tests
error[E0063]: missing field `goal` in initializer of `PersistentAgentDecl`
... (74×)

$ cargo clippy --workspace --all-targets -- -D warnings
error: function `percent_encode_query` is never used
     --> crates/caco-cli/src/lib.rs:21387:4
```

## After state

- Python script (window-scan with `min(i+5, len)` lookahead for `-->` line, per the standard pattern that's avoided silent misses) inserted `goal: None,` immediately before `depends_on_node: None,` at all 74 sites in `crates/caco-daemon/src/persistent.rs::tests`.
- `percent_encode_query` annotated with `#[allow(dead_code)]` + comment that it's a utility for future query-string-building call sites (the function is well-tested and worth keeping).
- All 74 fixtures + 1 dead-code site fixed; `cargo build -p caco-daemon --tests` clean; `cargo clippy --workspace --all-targets -- -D warnings` clean.

Verification:
- `cargo test-small`: 57/57 PASS
- `cargo clippy --workspace --all-targets -- -D warnings`: clean

## Diff summary

2 files changed, +149 / −0:

- `crates/caco-daemon/src/persistent.rs`: +148 / −0 (74 `goal: None,` insertions)
- `crates/caco-cli/src/lib.rs`: +1 / −0 (`#[allow(dead_code)]` annotation)

## Operator-takeaway

**This is the largest single broken-on-main wave I've repaired this session** (74 sites, 5× the previous record). It would have been caught by the merge-queue gate upgrade (bd-29bf2b) — but bd-29bf2b's gate runs on the **submitting** branch, not on main after-the-fact. Peer wmi-1's bd-6b2af8 must have:
1. passed locally (the field-add itself is non-breaking for its own fixtures)
2. passed the merge-queue gate (its own local fixtures had `goal: None` everywhere)
3. landed on main, breaking everyone else's fixtures that were authored before the field-add

This is exactly the case **bd-2c399b (queue daemon)** is designed to fix: serialized reintegration where each submission is **rebased + re-tested against current main** before push. bd-29bf2b's local-gate is necessary but not sufficient; bd-e5eec5's stale-base re-check is necessary but only catches the staleness window (it didn't catch this wave — the agent that submitted bd-6b2af8 wasn't stale, the agents that fetched main later were).

Filing as P2 follow-up: **bd-29bf2b's merge-queue gate should run on main, not just on the submitting branch** — i.e. after every successful merge, the gate should re-run on main as a post-condition check, and if it fails, file a high-priority broken-on-main bead automatically. Cost: ~2-3min per merge. Benefit: catches struct-field-add-without-fanout-fixture-update at source.

This cycle: **broken-on-main streak ended at 5** with two waves bundled in this repair. Reintegrate now to land before the next field-add lands.
