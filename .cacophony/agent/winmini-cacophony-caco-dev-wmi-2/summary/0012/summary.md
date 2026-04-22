# bd-09542b + bd-bf1e86 polish #7: doc-list fix + Profile fixture + confirmation-dialog ellipsis

## Goal

(a) Repair broken-on-main wave #11: rustdoc list-without-blank-line in `caco-daemon/src/choices.rs` (autonomy_tier doc) + missing `self_nudge_interval_secs` field on a `Profile{}` fixture site in `caco-cli`.
(b) Continue bd-bf1e86 polish track #7: confirmation-dialog message rendering previously hard-chopped at dialog width with no indicator. Operators could confirm destructive prompts without seeing the full text. Append `…` so the visual asymmetry warns them.

## Bead(s)

- bd-09542b (P1 broken-on-main, claimed + closed by reintegrate)
- bd-bf1e86 (P2 permanent, polish #7 — stays open)

## Before state

**bd-09542b — issue 1**: `crates/caco-daemon/src/choices.rs:42-50`. autonomy_tier doc has a 3-bullet list:
```rust
/// Free-form string; conventional values:
///   - "operator-only" (default; never auto-fired)
///   - "autonomous-low-risk" ...
///   - "autonomous-reversible" ...
/// Slice 1 ships the field only; the supervisor that consults it ...
```
rustdoc with `-D warnings` emits "doc list item without indentation" for the continuation paragraph because there's no blank doc-line separating it from the list. Same pattern fixed earlier this session in `caco-daemon/src/store.rs::PruneOutcome::note_delivery_tracking` per bd-274c2d log.

**bd-09542b — issue 2**: `crates/caco-cli/src/lib.rs:79595`. Profile fixture missing newly-added `self_nudge_interval_secs: Option<u64>`. E0063.

**bd-bf1e86 polish #7**: `crates/caco-tui/src/views/button.rs:725` — confirmation dialog rendered the message with `for (i, ch) in message.chars().take(max_msg_w).enumerate()`. A long message like `"This will permanently delete 47 beads including their attached agent checkouts and"` would render up to dialog width with no `…` — operators reading just the visible portion might confirm without realising the action's full scope.

This is polish #7 on bd-bf1e86. Polish #6 (inbox preview ellipsis) raised the question "where else does the codebase hard-chop without an indicator?" — `chars().take(N)` audit identified `button.rs` as the riskiest such site (logs.rs is line-wrap, correct as-is).

## After state

**bd-09542b**: 
- `choices.rs`: blank `///` line inserted between list and `"Slice 1 ships..."` paragraph. rustdoc clean.
- `lib.rs`: Python script (regex-bug fixed: extended search window to `min(i+5, ...)` instead of `i+1`) backfilled the missing field at the Profile fixture site.

**polish #7**: `button.rs` rendering now:
```rust
let render_chars: Vec<char> = if msg_chars.len() > max_msg_w && max_msg_w > 1 {
    let mut v: Vec<char> = msg_chars.iter().copied().take(max_msg_w - 1).collect();
    v.push('…');
    v
} else {
    msg_chars.into_iter().take(max_msg_w).collect()
};
```
For messages that fit, behaviour unchanged. For longer messages, the last visible char is replaced with `…` so the operator sees a clear indicator that the prompt continues beyond the dialog edge.

Verification:
- `cargo test-small`: 56/56 PASS
- `cargo clippy --workspace --all-targets -- -D warnings`: clean

## Diff summary

3 files changed, +15 / -1:

- `crates/caco-daemon/src/choices.rs`: +1 (blank doc-line)
- `crates/caco-cli/src/lib.rs`: +1 (Profile fixture field)
- `crates/caco-tui/src/views/button.rs`: +13 / -1 (ellipsis logic + comment)

## Operator-takeaway

**Wave 11** of broken-on-main this session. New observation: the cargo-error-driven Python script needs to look ahead more than 1 line for the `-->` location in `-D warnings` output (originally hard-coded `err_lines[i+1]`, which silently missed all sites — script reported "0 sites patched" while clippy still failed). Fixed by extending the search window to 5 lines. Worth keeping in mind if anyone else builds similar tooling.

The rustdoc list-paragraph rule keeps biting (twice this session, also fixed in bd-274c2d log earlier). Could be fixed at the codebase level by configuring rustfmt/just hooks to enforce a blank-line-after-list rule, but the rule is unusual enough that operators don't anticipate it. Filing a follow-up isn't worth it; manual fix-on-encounter is fine.

Polish #7 is the seventh cycle on bd-bf1e86 in this session. Pattern: each cycle adds 5-30 lines and closes one specific TUI rough edge. After 7 cycles, bd-bf1e86 has driven: poll-error UI, freshness indicator, 5 empty-state-hint surfaces, inbox preview ellipsis, confirmation-dialog ellipsis. Next polish cycle could:
- Audit other dialog-rendering sites for the same issue (modals in `views/modals.rs` if any).
- Pivot to a different theme (color/contrast accessibility, keyboard hint footers, etc.).
- Or pause polish and pick up a real ticket from `caco bd list`.
