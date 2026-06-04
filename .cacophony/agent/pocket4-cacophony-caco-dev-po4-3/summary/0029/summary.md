# bd-91b2a2 — ops cluster_freshness false-positive: adopt valid on-disk checkouts as initialized

## Bead
bd-91b2a2 (P3, task): `caco ops check` cluster_freshness persistently reported helsinki-hosted
non-cacophony project checkouts (a.skh.am, agent-utils, android-utils, collective, gfx-replacer, …) as
`operator_choice_required:checkout_not_initialized` with a recommended **destructive** action
(`caco @helsinki checkout regenerate --project <name>`) — even though the checkouts are present on disk with
valid HEADs. The condition appeared right after the helsinki daemon restart for the 1.2.1074 update and
persisted steady-state for 4.5h+ across many controller sweeps.

## Root cause
In `crates/caco-daemon/src/checkout.rs`, `init_project_state` registers a checkout with
`state.initialized = canonical_checkout_ready(&path)` (valid `.git`, non-placeholder, has `origin` + valid
HEAD). When that on-disk probe momentarily reports `false` for an intact checkout (e.g. a transient
`git remote get-url origin` / `git rev-parse HEAD` hiccup during a busy post-restart scan), init fell through
to the `manual_gc_required_status` fence, which emits `checkout_not_initialized` plus the destructive
`checkout regenerate` recommendation. That fence never re-evaluates the on-disk reality, so the false positive
stuck steady-state. The ops surface (`crates/caco-cli/src/ops_cmd.rs`, `checkout.cluster_freshness`) just
faithfully renders the daemon's `checkout_initialized: false`, so the fix belongs upstream in the daemon's
init/freshness tracker.

## Fix
Added a re-derivation guard in `init_project_state` (the `state.initialized == false` path), BEFORE the
`manual_gc_required_status` fence:

- If `state.path.exists() && canonical_checkout_ready(&state.path) && checkout_cache_mismatch_reason(state).is_none()`,
  adopt the checkout in place: converge declared `origin`/SSH/extra-remotes (same as the already-initialized
  path), persist checkout state, set `state.initialized = true`, warm the state branch, and report
  `initialized: true` with a normal health snapshot.

This satisfies the bead asks:
1. **Ask #1** — an on-disk checkout with a valid HEAD now counts as initialized without requiring a
   fetch/agent-launch pass to clear `checkout_not_initialized`.
2. **Ask #2** — the guard does NOT mask a genuinely-not-initialized condition: a placeholder (no usable origin)
   or a HEAD-less checkout fails `canonical_checkout_ready`, and a real identity-drift checkout is caught by
   `checkout_cache_mismatch_reason(...)`. Those still fall through to the operator-choice fence, so the
   destructive-reclone recommendation is reserved for checkouts that actually need it.

No change to the ops_cmd rendering or the `checkout regenerate` repair path; the false-positive simply stops
being produced at the source.

## Files
- `crates/caco-daemon/src/checkout.rs`
  - `init_project_state`: new bd-91b2a2 adopt-in-place guard (+~62 lines, with rationale comment).
  - Tests (`+~84` lines):
    - `reinit_adopts_valid_on_disk_checkout_marked_uninitialized_bd_91b2a2`: init a real checkout, force the
      registered `initialized=false` (simulating the transient mis-derivation) while asserting the on-disk
      checkout is genuinely ready, re-init, and assert it is re-recognized as initialized and NOT fenced with
      a `checkout regenerate` recommendation.
    - `reinit_still_fences_placeholder_marked_uninitialized_bd_91b2a2`: a placeholder (no usable origin) stays
      uninitialized across re-init — proving the guard does not over-adopt (ask #2).

Net diff: +146 lines, 1 file.

## Validation (queued per shared-host policy; caco-daemon lib needs 16 MiB stack)
- `RUST_MIN_STACK=33554432 CARGO_BUILD_JOBS=2 cargo test -p caco-daemon --lib bd_91b2a2 -- --test-threads=2`
  → 2 passed, 0 failed.
- `RUST_MIN_STACK=33554432 CARGO_BUILD_JOBS=2 cargo test -p caco-daemon --lib placeholder -- --test-threads=2`
  → 18 passed, 0 failed (no regression in placeholder/restart/init contracts).
- `cacophony-fast-tests` reintegration gate runs test-small + `cargo check --workspace --tests` + clippy.

## SPEC areas
Daemon canonical-checkout freshness/initialization tracking and the `caco ops` cluster_freshness operator
surface (avoiding false destructive-reclone recommendations for intact checkouts).

## Notes / out of scope
- Did not change `canonical_checkout_ready` itself (its `origin + HEAD` contract is correct); the fix makes
  init resilient to a transient false registration rather than loosening the readiness definition.
- The exact transient that produced the original `false` on helsinki was not reproduced on this headless node;
  the fix is a deterministic, defensible convergence guard that re-checks on-disk reality, which both removes
  the observed false positive and is robust to any future momentary mis-derivation.
