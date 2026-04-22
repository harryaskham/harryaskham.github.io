# bd-ff753c: caco agent prune --include-discarded + broken-on-main #12 bundle

## Goal

(a) Land the first slice of bd-ff753c worker-checkout cleanup: extend the existing `caco agent prune` retention sweep to optionally include Discarded agents (not just Completed), so failed/abandoned runs can be reclaimed under the same policy.
(b) Repair broken-on-main wave #12: missing `dispatch_operator_actions_list` function (wmi-1's bd-8fe920 dispatch wiring landed without the implementation) + a `daemon DB + logs` doc-comment in `dispatch_fleet_disk` that rustdoc parsed as a list-item-without-indentation.

## Bead(s)

- bd-ff753c (P2 worker-checkout cleanup feature; closes via reintegrate. First slice — full vision in bd description includes auto-trigger on discard/complete with safety period, deferred to follow-up)
- (broken-on-main wave #12 bead failed to file due to daemon restart — will file shortly via msg or follow-up bead)

## Before state

**bd-ff753c:**
- `caco agent prune --dry-run` filtered inventory to `state == Completed && !pruned && ended_at.is_some()`. Discarded agents (failed/abandoned) accumulated forever with no automated reclaim path.
- On this node tonight, prune reported 0 reclaimable GiB while the disk had 201 GiB of pressure including 11 GiB of Discarded checkouts that the policy would happily clean up if it could see them.

**broken-on-main #12:**
- `caco-cli/src/lib.rs:10150` calls `dispatch_operator_actions_list(project, json_requested)` but the function doesn't exist anywhere in src. wmi-1's bd-8fe920 reintegrate landed only the dispatch wiring + tests + COMMAND_TABLE entry. E0425.
- `caco-cli/src/lib.rs:50845` in `dispatch_fleet_disk` doc comment: `"daemon DB + logs"` — the literal `+` at start-of-doc-line was parsed by rustdoc as a list bullet, then the following 3 wrap-continuation lines flagged as "doc list item without indentation". Same pattern fixed earlier this session in `choices.rs::autonomy_tier` (bd-09542b) and `store.rs::PruneOutcome` (bd-274c2d log).

## After state

**bd-ff753c:**
- `plan_completed_checkout_retention_sweep` (caco-daemon/src/lib.rs:3076) no longer pre-filters to `Completed` only; it accepts whatever inventory the caller provides (still requires `!pruned && ended_at.is_some()` so age math is well-defined). The caller now owns the state filter.
- `dispatch_agent_prune` (caco-cli/src/lib.rs) signature gains `include_discarded: bool`. When `false`, behaviour is bit-identical to before. When `true`, Discarded agents flow into the inventory and go through the same hours/count/bytes policy as Completed.
- `AGENT_PRUNE_ARGS` gains `--include-discarded` arg-spec entry.
- New unit test `completed_retention_plan_includes_discarded_when_caller_provides_them`: 3-agent fixture (completed-old 72h, discarded-old 72h, discarded-fresh 1h) with 4-hour policy → asserts both -old agents are reclaimed, -fresh is not.
- **Live verified**: `caco agent prune --dry-run --include-discarded` on this node now reports 11 GiB across previously-invisible Discarded checkouts. Default `--dry-run` (without flag) reports same as before.

**broken-on-main #12:**
- `dispatch_operator_actions_list` implemented as a thin 6-line shim that builds a flags map (`--project=<resolved>`) and delegates to the existing `dispatch_bd_operator_actions(json_requested, &flags, co)` handler. No new business logic — the dashboard subsurface already exists at `bd operator-actions`; this just exposes it under the new `caco operator-actions list` command path peer wmi-1 wired.
- `dispatch_fleet_disk` doc-comment: replaced `"daemon DB + logs"` with `"daemon DB plus logs"`.

Verification:
- `cargo test-small`: 56/56 PASS
- `cargo test -p caco-daemon --lib completed_retention_plan`: 3/3 PASS (new test + 2 existing)
- `cargo clippy --workspace --all-targets -- -D warnings`: clean

## Diff summary

2 files changed, +114 / -7:

- `crates/caco-daemon/src/lib.rs`: +83 / -4 (test + filter relaxation)
- `crates/caco-cli/src/lib.rs`: +31 / -3 (arg spec, dispatch_agent_prune sig + filter, operator-actions shim, fleet_disk doc fix)

## Operator-takeaway

**bd-ff753c slice 1 of N**: the smallest useful step — flag-gated extension of the existing retention pipeline to cover Discarded agents. Operators get immediate disk-pressure relief by running `caco agent prune --dry-run --include-discarded` then `--include-discarded` (without --dry-run) to actually reclaim. On this node that's 11 GiB of recovery from one command.

The bead's broader vision still has follow-up scope:
- Auto-trigger checkout cleanup on `agent discard` / `agent complete` with safety period (e.g., 24h grace before disk reclaim eligible).
- Cross-node aggregation: `caco agent prune --node NAME --dry-run` to plan remote nodes from one place.
- Orphan detection: checkouts on disk with no corresponding agent record (could happen after manual deletes / corruption). Currently the retention sweep only sees agents from `AgentManager.list_all()`, which means orphans are invisible.

Each is a follow-up bead's worth of work. Filing them is itself a follow-up.

**Wave 12** of broken-on-main this session — same fundamental antipattern as bd-f4f4cd (peer landed dispatch wiring + tests without the function), and same doc-list-paragraph antipattern from bd-09542b. bd-29bf2b mixin-gate proposal would catch the missing-fn case (cargo build --workspace fails). Doc-list cases need cargo doc gating, separate scope.
