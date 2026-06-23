# Session summary — Hoist token_stats into shared caco-stats crate (bd-b18111 stage A)

## Goal

Begin the durable daemon token-usage pipeline (bd-2da2c5 slice 2). As the
bd-9b88a4 author I was harvested into bd-b18111 with the design distilled. This
chunk lands the structural foundation: move the existing token-usage
aggregation core out of `caco-cli` into a new shared `caco-stats` crate so the
forthcoming `caco-daemon` token-usage sink and collection loop can reuse the
same model + Pi-session parser without duplicating them. Landed separately to
de-risk the heavier daemon work.

## Bead(s)

- `bd-b18111` — [bd-2da2c5 s2] Daemon+agents: token-usage emission + collection sink + per-project aggregation
- (parent epic: `bd-2da2c5` — unified caco stats)
- (foundation: `bd-9b88a4` — token_stats core, already landed)

## Before state

- Failing tests: none
- `token_stats` lived only in `crates/caco-cli/src/token_stats.rs`, reachable as
  `caco_cli::token_stats` and consumed by `caco node tokens`.
- `caco-daemon` had no access to the parse/aggregate logic; reusing it daemon-side
  would have required duplication (the structural wrinkle flagged in the bead).

## After state

- Failing tests: none
- New `crates/caco-stats` crate (deps: serde + serde_json only) holds the
  `token_stats` module: `TokenUsage`, `TokenTotals`, `parse_pi_session_usage_line`,
  `project_from_cwd`, `aggregate_token_usage`, `read_pi_session_token_usage`,
  `default_pi_session_dir`, `grand_total`.
- `caco-cli` re-exports it as `caco_cli::token_stats` (thin `pub use`), so
  `caco node tokens` and all existing consumers compile unchanged.
- `caco-daemon` can now depend on `caco-stats` for the sink (next chunk).

## Diff summary

- Code/content commits: `b9d0cb635d` (pending final squash SHA from reintegration receipt)
- Summary artefact commit: intentionally omitted (no self-reference)
- Files touched: `crates/caco-stats/Cargo.toml` (new), `crates/caco-stats/src/lib.rs` (new),
  `crates/caco-stats/src/token_stats.rs` (moved via git mv from caco-cli),
  `crates/caco-cli/src/lib.rs` (re-export), `crates/caco-cli/Cargo.toml` (dep),
  `Cargo.toml` (workspace member), `Cargo.lock` (+10, minimal new-crate entry)
- Tests: +0 / -0 / flipped 0 (8 existing token_stats unit tests moved with the module, all pass)
- Behavioural delta: none — pure refactor; `caco node tokens` output unchanged.

## Operator-takeaway

The token-usage logic is now in a shared `caco-stats` crate that both the CLI
and the daemon can depend on. This unblocks the actual slice-2 work (daemon sink
+ collection loop + aggregation endpoint) without code duplication. Next chunk
adds the daemon-side append-only JSONL + SQLite sink and a periodic collection
loop that reads managed agents' Pi session dirs into it.
