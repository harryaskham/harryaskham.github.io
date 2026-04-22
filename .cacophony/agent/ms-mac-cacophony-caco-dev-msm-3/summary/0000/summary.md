# Session summary — prune feed.jsonl + undelivered.jsonl, surface in doctor

## Goal

Stop the daemon's two append-only JSONL ledgers (`feed.jsonl`,
`undelivered.jsonl`) from growing without bound. They were the
single largest source of invisible disk bloat in the daemon directory
(700 MB combined on helsinki) and were not surfaced by `caco doctor`.

## Bead(s)

- `bd-8469eb` — feed.jsonl and undelivered.jsonl grow unbounded — 510MB combined on helsinki

## Before state

- `feed.jsonl`: 367 MB / 709,877 lines / 17 days on helsinki, never pruned.
- `undelivered.jsonl`: 143 MB / 172,751 lines, only ever shrunk via successful retry / ack.
- `prune_retention` only touched SQLite tables (`feed_events`, `perf_events`, `exceptions`, etc.). The on-disk JSONL was orthogonal.
- `caco doctor` storage area surfaced `daemon.db / -wal / -shm` only. JSONL bloat was invisible.
- Failing tests: none (silent growth bug).

## After state

- `prune_retention` now also rewrites `feed.jsonl` (>14 d old lines dropped) and `undelivered.jsonl` (>7 d old lines dropped) via an atomic temp-file rewrite. Below 10 MB the file is left byte-identical (cost vs benefit). Lines whose timestamp cannot be parsed are preserved — we never silently lose ledger entries.
- `PruneStats` gained `feed_jsonl_{lines,bytes}` and `undelivered_jsonl_{lines,bytes}` for observability; the `Display` impl reports them.
- `caco doctor` storage area now surfaces both JSONL files at warning ≥ 100 MB and error ≥ 250 MB, with a recovery hint pointing at the retention path.
- Tests added: 3 in `caco-daemon::store::tests` (drops over-threshold, leaves under-threshold untouched, preserves unparseable) + 2 in `caco-cli` (classifier boundaries, doctor sparse-file integration).
- `cargo build -p caco-daemon -p caco-cli` clean. `cargo clippy -p caco-daemon -p caco-cli --lib` clean. New tests + the existing `store::tests::prune_*` suite (9 tests) all pass.

## Diff summary

- Commits: `fdb23beb`
- Files touched: `crates/caco-daemon/src/store.rs`, `crates/caco-cli/src/lib.rs`
- Tests: +5 (3 store-side prune, 2 doctor-side classifier+integration)
- Behavioural delta: every periodic `prune_retention` call now also bounds the two JSONL ledgers, and operators see them in `caco doctor` storage checks before they become painful.

## Operator-takeaway

Existing oversized files on long-running daemons (helsinki at 510 MB) will be pruned on the next prune cycle once the daemon restarts onto this version. If you want to free the bytes immediately, restart the daemon on the affected node — startup runs `prune_retention()`. Future bloat shows up directly in `caco doctor` rather than requiring `du -sh ~/.cacophony/daemon`.
