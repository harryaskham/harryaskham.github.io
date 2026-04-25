# Session summary — bd-ff7ff2 permanent auto-claim rollback

## Goal

Prevent a no-bead-id `caco bd claim` from leaving a worker falsely assigned to a permanent tracker if any stale or routed beads daemon still returns one despite the store-level skip added in `bd-a04b65`.

## Bead(s)

- `bd-ff7ff2` — caco bd claim still assigns permanent beads after bd-a04b65

## Before state

- `BeadsStore::claim_next_ready` on current `origin/main` already skipped `BeadStatus::Permanent` and had a regression test.
- In the live cluster, beadless `caco bd claim` still returned permanent tracker `bd-5bfb2c`, assigning it to this worker.
- The worker had to manually unclaim the permanent umbrella to avoid false implementation ownership.

## After state

- The caco-cli no-id claim path now defensively inspects a successful daemon response.
- If the daemon returns a `status: permanent` bead for a no-id claim, the CLI immediately calls the unclaim endpoint to roll back that accidental assignment and returns a `no_ready_beads` style error explaining that permanent trackers require explicit `--bead-id` claiming.
- Explicit permanent claims remain supported; the guard only fires when `bd claim` omitted a bead id.

## Diff summary

- Commits: pending reintegration commit for `bd-ff7ff2`.
- Files touched: `crates/caco-cli/src/lib.rs`.
- Tests: `cargo test -p caco-cli --lib auto_claimed_permanent_bead_message_treats_as_no_ready_work`; `cargo check -p caco-cli --tests`; `cargo test -p caco-beads claim_next_ready_skips_permanent_beads --lib`; `cargo fmt --all -- --check`; `git diff --check`.
- Behavioural delta: stale server-side permanent auto-claim responses no longer strand permanent trackers on workers.

## Operator-takeaway

Even if an old or routed beads daemon regresses the server-side permanent skip, the CLI now fails safe: it rolls the accidental permanent assignment back and tells the worker there is no implementation work to claim.
