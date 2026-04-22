# Session summary — caco msg inbox pagination flags (bd-1d476a)

## Goal

Stop `caco msg inbox` from dumping the entire unread history on every call. Add operator-friendly pagination flags + client-side filters. Pairs with bd-a7168d (server-side feed pop-in default-24h).

## Bead(s)

- `bd-1d476a` — caco msg inbox: missing pagination — no --tail / --since / --limit, returns full history.

## Before state

- Server-side `--limit / --offset / --kind / --since / --max-age / --include-system` existed but no operator-friendly defaults; calling `caco msg inbox --project X` dumped everything.

## After state

- New flags on `caco msg inbox`:
  - `--tail N` — alias for `--limit` when `--offset` is unset; defaults to **50** when neither is given so the dump is always bounded.
  - `--type KIND` — alias for `--kind`.
  - `--target PATTERN` — client-side substring match against `recipient` / `target`.
  - `--grep PATTERN` — client-side case-insensitive substring match against message body.
- Client-side filters live in a new `apply_inbox_client_filters()` helper. `total` is preserved (operators still see how many messages exist server-side); `count` is updated to reflect post-filter size.
- 4 new unit tests cover no-op, grep, target, and AND combination.
- Smoke check on live daemon: `caco msg inbox --project cacophony --tail 3` → `3 message(s) (of 201 total): …`.
- `cargo test-small` clean (204 / 109 / 720 / 291 / 18 / 2814 / 53). `cargo check --workspace --tests` clean.

## Diff summary

- Commit: `504cc12c`
- Files touched: `crates/caco-cli/src/lib.rs` (+171 / -3).
- Tests: +4 unit; 0 removed; 0 flipped.
- Behavioural delta: when called with no `--limit/--offset/--tail`, output is now capped at 50. Existing scripts that rely on full-history return must add `--limit 0` or `--limit 100000`.

## Out of scope

- `--since` accepting relative durations (`1h`, `30m`) — currently RFC3339 only; reuse the `parse_time_filter` helper from `beads.rs` when an operator asks.
- Server-side `--target` / `--grep` push-down — body filtering is fine client-side at the default cap of 50; existing server `--kind/--since/--max-age` are applied first so the wire payload stays small.

## Operator-takeaway

`caco msg inbox --project X` is now a sane default (last 50 messages). `caco msg inbox --tail 200 --target msm-2 --grep deploy` is the new triage primitive.
