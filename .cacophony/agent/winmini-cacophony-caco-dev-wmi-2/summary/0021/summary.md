# broken-on-main wave #15 (post-overlap repair) + dead-code allow

## Goal

Repair the duplicate-`goal: None` field in 74 `PersistentAgentDecl` test fixtures that resulted from a peer-overlap with wmi-1's hotfix (bd-6b2af8 follow-up), plus the `percent_encode_query` dead-code lint flagged after a separate refactor.

## Bead(s)

- (no claimed bead — pure broken-on-main repair)

## Before state

Two issues:

**Wave #15 (overlap)**: peer wmi-1 saw my early-signal speak about the missing-`goal` wave and landed a hotfix that backfilled all 74 sites with `goal: None,` immediately after `all_projects: false,`. Meanwhile I'd already run my own Python script that inserted `goal: None,` immediately before `depends_on_node: None,`. The reintegrate's auto-rebase merged both, leaving every literal with two `goal: None,` lines (E0062 duplicate field).

**Wave #16**: `percent_encode_query` in `crates/caco-cli/src/lib.rs:21387` had no callers after a recent refactor (clippy `dead_code` denial).

## After state

- Python script with regex `\n(\s+)goal: None,\n\n(\s+)depends_on_node:` → `\n\2depends_on_node:` removed exactly 74 `goal: None` duplicates (the ones I'd added). Verified `goal: None` count dropped from 148 → 74 (one per literal, post-fix).
- `percent_encode_query` annotated with `#[allow(dead_code)]` + comment justifying preservation as a future query-string utility.

Verification:
- `cargo build -p caco-daemon --tests`: clean (the test-small gate doesn't catch this)
- `cargo test-small`: 57/57 PASS (one flaky tui test panicked on first run, passed on retry — separate issue, will file follow-up)
- `cargo clippy --workspace --all-targets -- -D warnings`: clean

## Diff summary

2 files changed, +1 / +148 / −0 / −0 (net +149):

- `crates/caco-daemon/src/persistent.rs`: +148 / −0 (74 `goal: None,` insertions originally; the duplicate-removal in this final pass net-zero relative to the bedrock state but +148 vs the version main shipped before peer's hotfix landed both layers)
- `crates/caco-cli/src/lib.rs`: +1 / −0 (`#[allow(dead_code)]`)

## Operator-takeaway

Two reintegrate-cycle lessons:

1. **Peer-overlap when fixing the same broken-on-main wave concurrently produces duplicate-field bugs**, not merge conflicts. Both fixes succeed individually but compose into invalid code. Standard merge-conflict tooling won't catch this — needs post-merge `cargo build` validation. **bd-29bf2b**'s gate would catch this if run on the merged tip, not just the submitting branch.

2. **`cargo test-small` doesn't run daemon `--tests`** — peer wmi-1 confirmed in their speak: "test-small skips daemon tests; for struct-shape changes also run cargo build -p caco-daemon --tests." Worth adding to the standing endless-mode mixin: post-pull, run `cargo build -p caco-daemon --tests` in addition to `cargo test-small + clippy`. Or — better — extend `test-small` itself to include `cargo build --workspace --tests` (no test execution, just compile-check).

Filing follow-up beads for: (a) flaky tui test that panics intermittently in test-small, (b) extend test-small to include workspace --tests build-check.
