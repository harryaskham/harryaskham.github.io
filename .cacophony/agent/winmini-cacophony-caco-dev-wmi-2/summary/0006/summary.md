# bd-ab1c38: post-bd-7a1ca5 broken-on-main fallout (three waves)

## Goal

Restore main to a buildable, clippy-clean state after a sequence of peer-reintegration waves broke `cargo clippy --workspace --all-targets -- -D warnings` and `cargo test --lib` across `caco-beads`, `caco-daemon`, `caco-tui`, and `caco-cli`. Pure repair; no new feature surface.

## Bead(s)

- bd-ab1c38 (P1 broken-on-main, claimed + closed by reintegrate)
- bd-274c2d (P1 permanent test-health bead — appended a test-health-cycle entry covering this incident and the recurring peer-overlap pattern)

## Before state

Three consecutive broken-on-main waves landed on `origin/main` while this agent was rebasing/reintegrating:

**Wave 1** (post-bd-7a1ca5 agents_list_cache + agent_new dispatcher refactor):
- `caco-tui/src/app.rs`: 10 `AttachMetadata{}` fixture sites missing newly-added `tmux_history_limit` / `tmux_history_size` fields (E0063).
- `caco-daemon/src/ui_stream.rs`: 6 duplicate `tmux_history_*` assignments inside test fixtures from overlapping rebases (E0062).
- `caco-cli/src/lib.rs:77093`: duplicate `disable_hooks: None` inside a `CliReintegrationProfile` literal (E0062).
- `caco-cli/src/lib.rs:28968`: `dispatch_agent_new` grew to 15 parameters, tripping `clippy::too_many_arguments`.
- `caco-beads/src/sync.rs:771,818`: two `format!("static literal")` sites flagged by `clippy::useless_format`.

**Wave 2** (peer added `parent_bead_id` field to the `Bead` struct, backfill incomplete):
- `caco-beads/src/{model,store,sync}.rs` + `tests/store_integration.rs`: 92 `Bead{}` literal sites missing `parent_bead_id` (E0063).
- `caco-daemon/src/{audit,beads,lib,ui_stream}.rs`: 58 fixture sites missing the same field.

**Wave 3** (caused by Wave 1's dedup pass over-removing inside nested struct literals):
- `caco-tui/src/app.rs`: 1 `AttachMetadata` fixture lost its `tmux_history_*` lines when a sibling `AgentDisplayState` had them; 14 stray duplicates from overlapping merges still present.
- `caco-cli/src/lib.rs`: 3 `dispatch_agent_logs` test call sites need a new `since: Option<&str>` argument (peer added 5th param without updating callers).

`cargo test-small` passed only after Wave 3 cleanup; `cargo clippy --workspace --all-targets -- -D warnings` failed across all three waves until each was repaired.

## After state

Cargo-error-driven Python script applied repeatedly:
1. Run the failing build, capture errors to `/tmp/c.txt`.
2. Parse `--> path:line:col` anchors.
3. For each missing-field site, walk the source from `line` until the matching close brace (depth-tracking on `{`/`}`), then insert the missing field assignment at the appropriate indent before the close.
4. For duplicates, dedup with a window-scan that only removes a field if the same name is present earlier in the same `{...}` block (no `}` token in between).

Manual fixes:
- `dispatch_agent_new`: `#[allow(clippy::too_many_arguments)]` (defer builder refactor; dispatchers in this codebase are flat by convention).
- `dispatch_agent_logs`: regex-rewrite of 3 test call sites to insert `None` for the new `since` parameter.
- `caco-beads/src/sync.rs`: hand-edit two `format!("...")` literals into `"...".as_bytes().to_vec()`.
- Cherry-pick conflict resolution: chose theirs/ours per file based on equivalence (sync.rs is semantically identical between branches; summary content is ours; renamed to summary/0006/ since 0000 already taken upstream).

Verification:
- `cargo test-small`: 56/56 PASS
- `cargo clippy --workspace --all-targets -- -D warnings`: clean

## Diff summary

11 files changed, +213 / -12. Highlights:
- `crates/caco-beads/src/store.rs`: +59 (largest fixture density)
- `crates/caco-daemon/src/beads.rs`: +46
- `crates/caco-tui/src/app.rs`: +20 (Wave 1) -14 (Wave 3 dedup) +2 (Wave 3 re-add) net +8
- `crates/caco-daemon/src/lib.rs`: +8
- `crates/caco-beads/tests/store_integration.rs`: +7
- `crates/caco-daemon/src/ui_stream.rs`: -6 dedup, +6 backfill, net +0
- `crates/caco-cli/src/lib.rs`: +3 `None,` insertions, +1 `#[allow]`, -1 dup field
- `crates/caco-beads/src/sync.rs`: +6 / -3 (format! cleanup, then conflict-resolved to peer's variant)

## Operator-takeaway

Three consecutive broken-on-main waves in a single rebase cycle. All three caused by the same antipattern: **a peer adds a new required field to a widely-used struct (or a new required parameter to a widely-called function), but lands the change without a `git grep`-driven audit of every literal/call site, and the merge-queue gate (`cargo test-small` + clippy) doesn't catch it because most sites live in `--lib` tests outside that subset.**

Two recurring footguns to advertise (already logged separately in bd-274c2d):
1. Adding a new field to a widely-used struct should land with a `git grep '<StructName> {'` audit and a fresh `cargo test --lib --workspace` before reintegrate. The session-recording mixin should consider promoting this to a hook.
2. Multiple agents independently filing/claiming the same broken-on-main bead is wasteful — wmi-2 wasted ~30 min on a parallel fix to bd-bce6ea earlier this session. A short `caco msg speak --body 'starting bd-XXX'` ownership ping would resolve it cheaply; consider a soft-claim convention or file-touch lock under `.cacophony/agent/locks/`.

This bead exists as a discrete record of all three waves' specific damage and is closed at reintegrate.
