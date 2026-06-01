# Session summary — bd-cd40dc: canonical-main drift warning in caco doctor

## Goal

Complete the dev-lane diagnostic slice of bd-cd40dc (stale local canonical
daemon-checkout `main` refs causing per-node broken-on-main illusions): surface
local-canonical-checkout-`main` vs true-GitHub-remote-`main` drift as a read-only
freshness warning in `caco doctor`, matching the warning `caco ops` already
emits, so agents/operators can SEE a stale local mirror instead of silently
validating "is main broken?" against a lagging local ref.

## Bead(s)

- `bd-cd40dc` — Stale local canonical daemon-checkout main refs diverge from
  true GitHub remote, causing per-node broken-on-main illusions. P2 bug.
  (dev-lane slice; the `caco ops` half landed earlier via child `bd-643bd5`;
   the operator-territory replication/fetch root cause stays on the parent.)

## Before state

- Failing tests: none.
- `caco ops` already reported canonical-main-vs-true-remote drift
  (`collect_canonical_remote_drift`, landed by bd-643bd5 in PR #19), but
  `caco doctor` did NOT — the landed bd-643bd5 commit message explicitly says
  "add ... drift warning to caco ops" only, and a grep confirmed
  `collect_canonical_remote_drift` was referenced solely in `ops_cmd.rs`.
  The parent bead and the dev-slice acceptance (bd-6b73d3) named BOTH surfaces.

## After state

- Failing tests: none. 4 new caco-cli lib tests pass (validated via daemon
  queue): no-remote skip, dns-failed unknown, in_sync and behind against a local
  bare remote. `cargo check -p caco-cli` and `cargo clippy -p caco-cli --lib`
  clean (only residual warning is a pre-existing unused import in store.rs).
- New shared typed helper `CanonicalMainDriftSummary` +
  `canonical_main_remote_drift_summary` in `ops_cmd.rs` reuse the same bounded
  `ls-remote` probe and `classify_drift_state` classifier, so `caco doctor` and
  `caco ops` agree without coupling doctor to the ops JSON shape.
- `caco doctor` now emits a per-project `checkout '<p>' canonical main drift`
  check: ok (in_sync) / warning (behind|diverged, with a hint not to validate
  broken-on-main against the local ref) / info (skipped: no remote) / unknown
  (probe inconclusive). Strictly read-only; local-mirror refresh stays
  operator/caco-ctrl owned.

## Diff summary

- Code/content commit: 7912d53e570089094cd0c2e31ad37c29ef75b48a (final landed
  squash SHA from the reintegration receipt).
- Files touched: `crates/caco-cli/src/ops_cmd.rs` (+202: typed summary helper +
  4 tests), `crates/caco-cli/src/lib.rs` (+62: doctor drift check),
  `AGENTS.md` (+1 line noting both surfaces report the drift warning).
- Tests: +4 (all passing). No Rust tests changed.
- Behavioural delta: `caco doctor` gains a read-only canonical-main drift
  freshness signal per project; no mutation, no fetch, bounded probe.

## Operator-takeaway

The reintegration/rebase machinery was never the bug — it self-corrects against
the true remote. The bug was stale local READ surfaces. The fix is purely
diagnostic: both `caco ops` and now `caco doctor` flag when a node's local
canonical `main` lags the real GitHub tip, so a stale mirror stops producing
phantom broken-on-main verdicts. The remaining open scope on parent bd-cd40dc —
WHY local mirrors fall behind (fetch cadence/replication) and the recovery path —
is operator/caco-ctrl territory and intentionally NOT in this slice.
