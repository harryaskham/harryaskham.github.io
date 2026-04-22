# bd-bf1e86 polish #8: beads empty-state keystroke hint + broken-on-main wave #13 repair

## Goal

(a) Polish track #8: extend the empty-state keystroke-hint pattern (already covering inbox/events/notifications/feed/merge_queue) to the beads view. When a project has zero beads, the UI now tells the operator the keys to make one or refresh.
(b) Repair broken-on-main wave #13: peer added `annotation: Option<String>` field to `AgentInfo` (bd-4a9bf4 free-form operator annotation) — my new `completed_retention_plan_includes_discarded_when_caller_provides_them` test fixture (landed in the same cycle as bd-ff753c) didn't have the field. Also delete the now-unused `dispatch_operator_actions_list` function — peer wmi-1 wired the dispatch arm directly to the existing `dispatch_bd_operator_actions` handler, leaving my function (added in earlier rebase to fix wave #12) as dead code.

## Bead(s)

- bd-bf1e86 (permanent polish bead — polish #8 of N this session)
- (broken-on-main wave #13 — drive-by; bundled with the polish since both are tiny)

## Before state

**Polish #8:** beads empty-state in `crates/caco-tui/src/views/beads.rs::render_empty` showed:
```
No beads
Project "X" has no open or recent beads.
```
No keystroke hint. The other 5 surfaces patched earlier this session all close with a "Press `X` to do Y" hint line; beads was the outlier.

**Wave #13:**
1. `crates/caco-daemon/src/lib.rs:41767` — my `mk` helper in the new test (`completed_retention_plan_includes_discarded_when_caller_provides_them`) constructs an `AgentInfo` literal that's missing the new `annotation` field added by peer wmi-1's bd-4a9bf4. E0063.
2. `crates/caco-cli/src/lib.rs:83152` — `dispatch_operator_actions_list` exists but is never called. Earlier in this cycle I'd added it as a thin shim to fix wave #12; peer wmi-1 then landed a different fix (call `dispatch_bd_operator_actions` directly) which superseded my approach. Result: dead code, clippy `dead_code` denial.

## After state

**Polish #8:** `render_empty` body now ends with
```
Press `b` or `N` to create a bead, `r` to refresh.
```
Both keys verified live in `caco-tui/src/app.rs` (`b` is the global "create bead from anywhere" key per bd-aa6b1c/bd-249b5c, `N` is the in-list "create bead" key per bd-10yj). No tests assert the body text directly, so no test churn.

**Wave #13:**
1. Added `annotation: None,` to the test-fixture `AgentInfo` literal at the same indentation as the other `None` fields.
2. Deleted `dispatch_operator_actions_list` (97 lines including doc-comment) — the dispatch arm at line 10236 already calls `dispatch_bd_operator_actions(json_requested, &parsed.flags, co)` directly, exactly what my shim would have done.

Verification:
- `cargo test-small`: 56/56 PASS
- `cargo clippy --workspace --all-targets -- -D warnings`: clean
- `cargo test -p caco-daemon --lib completed_retention_plan`: 3/3 PASS (Discarded test still passes after annotation field add)

## Diff summary

3 files changed, +7 / −98:

- `crates/caco-tui/src/views/beads.rs`: +6 / −0 (empty-state hint line)
- `crates/caco-daemon/src/lib.rs`: +1 / −0 (annotation: None in test fixture)
- `crates/caco-cli/src/lib.rs`: +0 / −98 (delete dead `dispatch_operator_actions_list`)

## Operator-takeaway

**Polish #8** completes the empty-state keystroke-hint pattern across all 6 list-style TUI surfaces this session. Pattern is now uniform — empty inbox tells you about `r` to refresh, empty beads tells you about `b`/`N` to create + `r` to refresh, etc. Net effect: discoverability for new operators looking at an empty pane and wondering "is the daemon broken or did I do something wrong?"

**Wave #13** is the second wave that would have been caught by **bd-29bf2b** (merge-queue gate upgrade from `cargo check` to `cargo test --lib --workspace`) which peer wmi-1 closed during this cycle. The struct-field-fixture-breakage class is exactly what 29bf2b targets — future cycles should see this drop from "every-cycle" to "rare". The dead-code wave (dispatch_operator_actions_list) is a different shape — peer-overlap on the same fix area — which still slips through 29bf2b's gate (since both fixes individually compile + test-pass) but is harmless to clean up post-rebase.

Of note: this cycle continues a pattern where I do small useful work AND the cycle itself surfaces one or two broken-on-main waves to mop up, all in one reintegrate. The wave-mop-up tax is paid once per polish, not zero, but smaller every cycle as the merge-queue gate improves.
