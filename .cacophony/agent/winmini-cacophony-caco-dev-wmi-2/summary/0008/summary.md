# bd-f72c32 + bd-bf1e86 polish #3: SessionKickedModal backfill + inbox empty-state keystroke hints

## Goal

(a) Repair the latest broken-on-main wave (4 `SessionKickedModal{}` fixture sites missing newly-required `tmux_history_*` fields).
(b) Continue the bd-bf1e86 polish track: when an inbox section is empty, tell the operator how to leave it. Operators routinely conclude "the inbox is broken" when really they're parked on a quiet tab.

## Bead(s)

- bd-f72c32 (P1 broken-on-main, claimed + closed by reintegrate)
- bd-bf1e86 (P2 permanent, polish #3 — stays open)

## Before state

**bd-f72c32:** `cargo test-small` and `cargo clippy --workspace --all-targets -- -D warnings` both failed at E0063 with 4 sites in `crates/caco-tui/src/app.rs` (lines 53594, 53638, ~+45/+85): every `SessionKickedModal { … }` literal lacked the `tmux_history_limit` / `tmux_history_size` fields that the modal struct now declares as required. Same pattern as bd-bce6ea / bd-ab1c38; fundamental mitigation tracked at bd-29bf2b (mixin gate upgrade — already claimed by ms-dev-msd-4).

**bd-bf1e86:** Empty inbox sections rendered as a single dim line — `"  No choices"`, `"  No direct messages"`, `"  Inbox is empty"`, etc. — with no indication that:
- ←/→ cycle through the other sections (which may have items).
- `Tab` toggles the archived view.

This is the polish-#3 cycle on bd-bf1e86. Polish #1 was the inbox poll-error UI; polish #2 was the merge-queue freshness indicator. The bead is permanent and explicitly framed as "a place to land continuous TUI papercut fixes".

## After state

**bd-f72c32:** Cargo-error-driven Python script applied (`/tmp/cw3.txt` → walk close braces → insert before): 4 `SessionKickedModal{}` literals backfilled with `tmux_history_limit: None, tmux_history_size: None`.

**bd-bf1e86 polish #3:** Empty-state rendering now emits two paragraphs separated by a blank line:

```
  No direct messages

  Press ←/→ to switch section, or Tab to view archived.
```

Per-section variants:
- `Choices`, `DirectMessages`, `Broadcasts`, `Speech`: hint mentions both ←/→ section cycling and `Tab` archived toggle.
- `All`: hint mentions only `Tab` (no other sections to switch to that aren't already aggregated).
- `inbox_show_archived` view: hint mentions only `Tab` to return to active inbox.

Verification:
- `cargo test-small`: 56/56 PASS
- `cargo clippy --workspace --all-targets -- -D warnings`: clean

## Diff summary

2 files changed, +45 / -8:

- `crates/caco-tui/src/app.rs`: +8 (4 paired field insertions for SessionKickedModal)
- `crates/caco-tui/src/views/inbox.rs`: +37 / -8 (per-section empty-state with hint paragraphs)

## Operator-takeaway

Two flavors of the same recurring footgun in one commit:
1. **Mechanical (bd-f72c32)** — 4 more sites broken by the same struct-evolution-without-grep antipattern. Sixth wave in this session. Mitigation already in flight at bd-29bf2b; nothing to add.
2. **Discoverability (bd-bf1e86 polish #3)** — empty list rendered as a single line is an information-vacuum. Adding a one-line keystroke hint costs almost nothing and removes the "is this broken?" question entirely. Same pattern should be applied wherever the TUI says "(no data)" or "is empty" without telling the operator how to do something useful next. Candidates for follow-up polish: `Cluster > Merge Queue ─ (no data)` (no hint), events timeline `"  No events yet..."` (no hint), feed view empty (no hint), notifications view empty.

bd-f72c32 closes at reintegrate. bd-bf1e86 stays open for polish #4 — likely "propagate empty-state hints to the other surfaces listed above".
