# Session summary — bd-9b88a4: surface the token-usage foundation via `caco node tokens`

## Goal
Ship the clean additive headless piece of bd-9b88a4 (caco stats slice 2): make msd-4's landed token_stats foundation operator-visible via a thin `caco node tokens` subcommand — per-project token usage from this node's local Pi session logs — WITHOUT forking bd-2da2c5's top-level `caco stats` noun (ctrl-endorsed approach).

## Bead(s)
- `bd-9b88a4` (P1) — caco stats slice 2: fleet token-usage emission/collection/aggregation. This slice surfaces sub-slice (c). The durable (a) per-turn emission + (b) daemon collection remain coordination-gated (Pi/Claude runtime owners + bd-2da2c5 surface) — explicitly deferred.

## Before state
- Failing tests: none.
- msd-4 landed the token_stats foundation (read_pi_session_token_usage + aggregate_token_usage, 7/7 tested) but it was UNSURFACED (only `pub mod token_stats;`, unused). No top-level `caco stats` noun (only `bd stats`/`msg stats`).

## After state
- Failing tests: none.
- New `caco node tokens` subcommand (under the existing `node` noun, mirroring `caco node disk`): reads this node's Pi session JSONL → per-project token totals (input/output/cache_read/cache_write/total/turns) + grand total. Table + `--json` ({ok,data:{projects,grand_total,session_dir},meta:{count}}). `--session-dir` override.
- New tested pure helper `token_stats::grand_total`. Handler `dispatch_node_tokens` is thin wiring.
- Validation (queued, all green): cargo check --workspace --tests (tj-854fd9c8); cargo test -p caco-cli --lib token_stats = 8/8 incl grand_total_sums_all_projects_bd_9b88a4 (tj-ce368178); cargo clippy -p caco-cli clean (bj-3154c331).

## Diff summary
- Commit: pending final squash SHA from the reintegration receipt.
- 2 files: crates/caco-cli/src/lib.rs (+97: NODE_TOKENS_ARGS, `tokens` CommandSpec, dispatch arm, dispatch_node_tokens handler), crates/caco-cli/src/token_stats.rs (+33: grand_total fn + test).
- Behavioural delta: new read-only operator command; no change to existing behavior. Deliberately NOT a top-level `caco stats` noun (bd-2da2c5's domain).

## Operator-takeaway
Ships immediate operator-visible per-project token usage (from local Pi sessions) consuming msd-4's validated foundation — zero rework-risk, no unvalidated daemon sink, no surface-design collision with bd-2da2c5. The durable fleet emission/collection (sub-slices a/b) stays coordination-gated for the Pi/Claude runtime owners + bd-2da2c5's unified-surface owner. Picked + scoped per ctrl after the EPIPE arc; read-the-history-first avoided rework.
