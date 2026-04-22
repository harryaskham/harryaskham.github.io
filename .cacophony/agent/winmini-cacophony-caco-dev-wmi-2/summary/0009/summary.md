# bd-f4f4cd + bd-bf1e86 polish #4: dedup-flags fn stub + useless_format + empty-state hints

## Goal

(a) Repair broken-on-main wave #7 of this session: missing `validate_dedup_apply_flags` function in `caco-cli` (tests landed without the impl) + a `useless_format` clippy violation in `caco-daemon`.
(b) Continue bd-bf1e86 polish track: propagate the empty-state keystroke/context hints from polish #3 (inbox) to the events timeline and notifications panel. Both previously rendered as a single dim line with no context.

## Bead(s)

- bd-f4f4cd (P1 broken-on-main, claimed + closed by reintegrate)
- bd-bf1e86 (P2 permanent, polish #4 — stays open)

## Before state

**bd-f4f4cd:**
1. `crates/caco-cli/src/lib.rs:57441-57480` had 5 unit tests calling `validate_dedup_apply_flags(apply, interactive, json) -> Result<(), String>` plus 8 E0425 errors at compile time — the function was never landed in src. Peer's bd-fa4eb9 reintegrate dropped only the test scaffolding.
2. `crates/caco-daemon/src/beads.rs:3238`: `format!("duplicate_of and admin_override are mutually exclusive (bd-0c9836); pick one path")` flagged by `clippy::useless_format` (no formatting args).

**bd-bf1e86:**
- `crates/caco-tui/src/views/events.rs:307`: empty timeline rendered only `"  No events yet..."` — no indication of what feeds the timeline.
- `crates/caco-tui/src/views/notifications.rs:27`: empty notifications panel rendered only `"  No notifications"` — no indication of what populates it. Operators routinely asked "is this broken or just quiet?"

This is polish #4 on bd-bf1e86, building on:
- Polish #1 (already landed): inbox poll-error UI surfacing.
- Polish #2 (already landed): merge-queue panel "updated Ns ago" freshness suffix.
- Polish #3 (already landed): inbox empty-section keystroke hints.

## After state

**bd-f4f4cd:**
1. Defined `validate_dedup_apply_flags` with the exact contract the tests pin:
   - `interactive && !apply` → `Err("--interactive requires --apply")`.
   - `interactive && json` → `Err("--interactive cannot be combined with --json")`.
   - All other combinations → `Ok(())`.
   Marked `#[allow(dead_code)]` with a comment pointing at bd-fa4eb9 — the dispatcher that will eventually call this function — so the lint doesn't trip while the dispatcher catches up. 5 tests pass.
2. Replaced `format!()` call with bare string literal in `caco-daemon/src/beads.rs:3238`.

**bd-bf1e86 polish #4:**
- `events.rs`: empty timeline now renders three lines:
  ```
    No events yet

    This timeline fills as beads are claimed/closed, agents start/stop, or config/daemon state changes.
  ```
- `notifications.rs`: empty notifications panel now renders three lines:
  ```
    No notifications

    Daemon-side alerts (failed agents, capacity warnings, config drift) appear here.
  ```
- Existing notifications smoke test (`render_shows_empty_state`) still asserts on row 1 (`"No notifications"`) and continues to pass since the headline didn't move.

Verification:
- `cargo test-small`: 56/56 PASS
- `cargo test -p caco-cli --lib validate_dedup_apply_flags`: 5/5 PASS
- `cargo clippy --workspace --all-targets -- -D warnings`: clean

## Diff summary

4 files changed, +38 / -11:

- `crates/caco-cli/src/lib.rs`: +17 (new fn + #[allow] + doc comment)
- `crates/caco-daemon/src/beads.rs`: +1 / -3 (format! collapse)
- `crates/caco-tui/src/views/events.rs`: +9 / -4 (timeline empty-state)
- `crates/caco-tui/src/views/notifications.rs`: +11 / -4 (notifications empty-state)

## Operator-takeaway

**Wave 7** of broken-on-main this session. New variant of the recurring antipattern: peer landed test-only changes for a function that didn't exist yet (vs. previous waves where peers landed src-side struct/fn changes without backfilling fixtures). Same underlying gap — `cargo test-small + clippy` doesn't run `--lib` workspace-wide as part of the merge gate. Mitigation already in flight at bd-29bf2b.

The empty-state hint pattern from polish #3 generalizes well — same template applies anywhere the TUI says "(no data)", "is empty", "No X". Polish #5 candidates: feed view (`"  Waiting for events..."` — no follow-up context), merge-queue `(no data)` arm (already has freshness suffix from polish #2 but no explanatory hint).

bd-f4f4cd closes at reintegrate. bd-bf1e86 stays open for polish #5.
