# bd-bf1e86 polish #6: inbox preview uses common::truncate

## Goal

Pivot away from empty-state hint cycles (#3-#5 covered inbox, events, notifications, feed, merge_queue). Pick a different rough edge: the inbox list's body preview was hard-chopping at character 30 with no ellipsis, so operators couldn't tell whether a message ended at the cut point or had more content not shown.

## Bead(s)

- bd-bf1e86 (P2 permanent, polish #6 — stays open)

## Before state

`crates/caco-tui/src/views/inbox.rs:483-484`:
```rust
let body_preview: String = item.body.chars().take(30).collect();
let body_preview = body_preview.replace('\n', " ");
```

For a 30+ char message, the preview rendered exactly 30 chars with no indicator — `"the daemon failed to start beca"` looks indistinguishable from a complete short message. Meanwhile `common::truncate(s, max)` already does the right thing (appends `"..."` when truncating, preserves short strings as-is) and is used widely elsewhere in the same file (e.g. `sender_short = common::truncate(&caller_parts.id, 12)` two lines later).

## After state

```rust
let body_full = item.body.replace('\n', " ");
let body_preview = common::truncate(&body_full, 30);
```

Now a 30+ char message renders as `"the daemon failed to start b..."` (27 chars + `...`). 30-or-fewer char messages render unchanged.

Verification:
- `cargo test-small`: 56/56 PASS
- `cargo clippy --workspace --all-targets -- -D warnings`: clean
- Existing `common::truncate_*` unit tests in `views/common.rs:2338` already cover both branches (short string unchanged; long string truncated with `...`).

## Diff summary

1 file changed, +5 / -3:

- `crates/caco-tui/src/views/inbox.rs`: switch body_preview to use shared truncate helper.

## Operator-takeaway

Tiny but real fix — third spot today where the principle "operators need a visual indicator that information was elided" mattered. This time it was content truncation (not empty-state); same theme. The shared `common::truncate` helper exists and is already used elsewhere in the same file two lines later, so the pre-fix code was just inconsistent rather than missing infrastructure.

Other `chars().take(N)` sites worth auditing for the same pattern:
- `views/button.rs:725` (button label rendering — needs visual check; might be fine because button width is bounded by layout).
- `views/logs.rs:450` (log line wrap — likely correct because it's mid-wrap not end-truncate).

Polish #7 candidate: scan logs.rs/button.rs above to confirm or fix; or pivot again — six cycles on bd-bf1e86 in one session is plenty.
