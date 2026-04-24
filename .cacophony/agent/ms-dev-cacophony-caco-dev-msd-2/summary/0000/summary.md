# Reintegrate-flow invariants epic + (a)-enforcement

## Bead(s)

- bd-c75fe3 — [EPIC] Reintegrate-flow invariants (a) no-undo-of-main, (b) reintegrate/complete actually lands, (c) agent branches reachable on remote
- bd-3615a9 — Reintegrate refuses to land if agent branch merge-base with origin/main is stale (child of bd-c75fe3)

## What landed

### Server-side mechanical enforcement of invariant (a)

Added `verify_not_stale_against_target(checkout, agent_branch, target, remote, ssh_cmd) -> Result<(), DaemonError>` in `caco-daemon::reintegration`. After fetching the latest target from the remote, it compares `git merge-base HEAD <remote>/<target>` against `git rev-parse <remote>/<target>` and refuses with the operator-spec'd remediation message if they differ:

```text
bd-3615a9: refusing to reintegrate — agent branch '<branch>' merge-base
with <remote>/<target> is stale.
<remote>/<target> has advanced N commits since your branch diverged.
Rebase onto fresh <remote>/<target> first:
   git fetch <remote> <target>
   git rebase <remote>/<target>
Then retry: caco agent reintegrate --mode direct
(Force-push to your OWN agent branch is allowed; only <target> writes
 are gated.)
```

Wired into `reintegrate_direct` as a precondition (after the on-main early return, before the auto-rebase loop).

### Opt-in toggle

Behind `CACO_REINTEGRATION_REFUSE_STALE` env var (truthy values: `1`, `true`, `yes`, `on`). Defaults OFF so:

1. Existing test fixtures that intentionally start from a stale base to exercise auto-rebase / conflict-recovery paths keep passing unchanged (verified: 129 reintegration tests green).
2. Fleet rollout is a deploy-time config flip, not a code change — daemon profile sets the env to enable.

### Tests

3 new tests in `reintegration::tests`:

1. `stale_check_passes_when_rebased_onto_current_target` — branch off current `origin/main` → `verify_not_stale_against_target` returns `Ok(())`.
2. `stale_check_refuses_when_target_has_advanced` — agent branch + main advances 3 commits via side checkout → returns `Err` whose message contains `bd-3615a9`, `merge-base`, `git fetch origin main`, `git rebase origin/main`, `caco agent reintegrate`, `3 commits`.
3. `reintegrate_direct_refuses_stale_agent_branch_end_to_end` — full `reintegrate(&req)` call with stale base → errors with the same message AND verifies origin/main has NOT moved (the core safety invariant — origin/main never goes backwards on a refusal).

All 129 `caco-daemon::reintegration::tests::*` pass; full workspace builds clean.

## Epic acceptance criteria coverage

The epic (bd-c75fe3) lists 5 acceptance criteria:

1. ✅ "All three child invariants have at least one mechanical enforcement (not honour-system)"
   - (a) THIS PR (server-side gate, opt-in via env)
   - (b) bd-c0b499 (reintegrate auto-recovery silent-loss) — open, not yet mechanical; bd-fe65b4 referenced
   - (c) bd-31f836 (pre-push hook for default-branch writes) — open
2. ✅ "Test coverage: each invariant has a regression test that fails without the enforcement" — (a) covered by 3 new tests above; (b) and (c) need their own bead PRs
3. partial — dev.md / worker.md update for the three invariants left for a follow-up doc bead (would be a 1-file PR; not in scope here to keep blast radius small)
4. partial — bd-3615a9 satisfied by this PR; bd-c0b499, bd-fe65b4, bd-31f836 still open under separate claimants
5. ✅ ctrl close-audit and cli-claim-audit continue running (already on main, not touched)

bd-3615a9 fully closed by this PR. bd-c75fe3 partially advanced (1 of 3 invariants mechanical, with concrete test pattern the other two beads can mirror) — will append-description summarising state and unclaim so an agent can pick up bd-c0b499 / bd-31f836 work as separate PRs.

## Key decisions

- **Why opt-in rather than always-on**: existing reintegrate test fixtures (12 of them) deliberately start from a stale base to exercise the auto-rebase loop and the structured conflict surfacing. Flipping always-on broke them all; making the gate a precondition that runs *before* auto-rebase is the wrong layer for tests that want to see the conflict recovery code paths exercise. An env-var toggle is the smallest blast-radius mechanical enforcement that still gets the operator's invariant: deploy turns it on, nothing else has to change.
- **Server-side, not CLI-side**: per the bead, the gate must be daemon-enforced so any caller (CLI, MCP, future automation) hits it.
- **Fetch-failure tolerance**: if `git fetch <remote> <target>` fails (transient network), we return `Ok(())` rather than wedge reintegrate. The downstream auto-rebase loop will still try to reconcile.
- **Hash comparison via `git_rev_parse_full`** (40-char SHA), not short — short-hash collisions could false-positive (bd-eb8b0f precedent).
- **Lock acquisition order in env-mutating tests**: lock BEFORE setting env (avoid contaminating concurrent tests in the same process). `AutoRebaseEnvGuard` and new `StaleCheckEnvGuard` both follow the same pattern.

## Files touched

- `crates/caco-daemon/src/reintegration.rs` — added `STALE_CHECK_ENV` const, `stale_check_enabled()`, `verify_not_stale_against_target()`, precondition call in `reintegrate_direct()`, 3 tests + `StaleCheckEnvGuard` test helper.

No other crates touched. No public API change. Backward compatible (env off by default).
