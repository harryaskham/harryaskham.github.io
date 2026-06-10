# bd-caf4f6 — Auto-reconcile state-branch checkout origin remote on config change

## Bead
bd-caf4f6 (beads/checkouts/config-distribution/state-branches, P2; operator-flagged, filer caco-ctrl). Changing a project's config remote required manual per-node `git remote set-url origin` surgery across every node and checkout type (Harry's 11-node loop to fix picasso + mono state-branch remotes). Same "daemon owns its checkouts' git state" family as bd-cd9125 (stale-lock reaping) and bd-396f9f (reintegration temp-dir sweep).

## Root cause (the state-branch gap)
On config reload, `config_reload` already calls `CheckoutManager::reconcile_config` → `init_all` → `init_project_state`, which converges the CANONICAL checkout's origin to the new config remote (`configure_origin_remote(&state.path, &state.remote)`, bd-a01c3f, line ~2141) and warms the state-branch via `warm_state_branch_checkout`. But the state-branch warm path, on an EXISTING (ready) dest, only set the SSH command and then fetched from `"origin"` — it never reconciled that existing dest's origin URL to the (possibly changed) `state.remote`. So the state-branch checkout kept pointing at the clone-time URL after a config remote change → exactly the manual `set-url` surgery the operator had to do.

## Change (crates/caco-daemon/src/checkout.rs)
In `warm_state_branch_checkout_with_timeout`'s existing-dest path, before the `fetch_branch`, converge the dest's origin (and declared extra remotes) to the current config remote — mirroring the canonical converge-on-refresh:
- `state_branch_origin_needs_reconcile(actual, expected)` (new, normalize-aware via the existing `normalize_git_url`): equivalent GitHub spellings (`git@github.com:o/r` ≡ `ssh://git@ssh.github.com:443/o/r.git`) are a no-op; a genuinely different remote triggers reconcile.
- On a real change: `configure_origin_remote(&dest, &state.remote)` + a `bd-caf4f6: reconciled state-branch origin for '<project>' at <path>: <old> -> <new>` log line; failures logged non-fatally.
- `configure_extra_remote` for each declared extra remote (idempotent).

Fires automatically on config reload (reconcile_config → init_all → init_project_state → warm_state_branch) AND on the periodic checkout refresh, on every node — eliminating the manual per-node surgery. Idempotent, operator-visible/logged, scoped to the daemon-owned state-branch dest, and topology-safe (origin = `project.remote`, which config-validation already refuses to bind to a push:forbidden URL, bd-a01c3f).

## Scope / follow-up
- Canonical checkout: already converged on refresh/reload (bd-a01c3f) — unchanged.
- State-branch checkout: fixed here (the dominant operator pain).
- Beads git store: NOT covered (structurally separate git store); filed as a follow-up.

## Validation (daemon test queue)
- `cargo test -p caco-daemon --lib caf4f6` (tj-9a13b800): PASSED — `state_branch_origin_needs_reconcile_normalize_aware_bd_caf4f6` (equivalent spellings = no-op; different repo/host = reconcile, incl. the picasso GHE remote case).
- `cargo clippy -p caco-daemon --lib` (tj-de7f24e8): checkout.rs clippy-clean; the 1 remaining warning is the pre-existing unrelated agent/lifecycle.rs:10401 (`was_non_terminal`), not gate-blocking.
- rustfmt-clean on changed regions; `git diff --check` clean.

## Diff
See the reintegration receipt for the landed squash SHA.
