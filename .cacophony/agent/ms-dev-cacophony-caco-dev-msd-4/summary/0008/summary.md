# Summary 0008 — bd-6da2c1: Reintegrate post-verification race fix

## Bead
bd-6da2c1 — Reintegrate post-verification race: first attempt may report failure when merge actually landed.

Filed by this agent earlier in session 0007 after observing the race twice
during my own bd-fbe1d7 / bd-dbf1b3 reintegrations. Distinct from
bd-f76c81 (summary-only branch path).

## Problem

`caco agent reintegrate --mode direct` occasionally reported failure on the
first invocation:

  reintegration failed: <bead-id>: reintegration inner steps succeeded but post-verification failed: ...

A subsequent `git fetch origin main` then showed the squash-merge commit
WAS already present on origin/main with the expected agent's branch
contents. Re-running reintegrate after a `git rebase origin/main` then
succeeded (or failed idempotently with a clear 'no changes to merge').

Root cause: both verification sites in `crates/caco-daemon/src/reintegration.rs`
fetched once and then immediately ran `git merge-base --is-ancestor`. If the
remote ref-cache had not yet propagated the new tip when the daemon's first
probe arrived (push 200-OK but post-receive publish lagging by a beat),
reachability returned false and the outcome was reported as failed despite
the work having genuinely landed.

## Change

`crates/caco-daemon/src/reintegration.rs`:

- New helper `verify_commit_reachable_polling(checkout, commit, remote, target, git_ssh_command)`:
  bounded-budget polling around `verify_commit_reachable`. First probe runs
  immediately so the happy path keeps existing latency; subsequent probes
  re-`git fetch` and re-check at 250ms intervals until either the commit
  becomes reachable or the budget expires. Default budget 5s, override via
  `CACO_REINTEGRATION_VERIFY_POLL_MS`.
- Outer `verify_direct_outcome` (line ~689): swapped the single
  fetch+probe for the polling helper.
- Inner `reintegrate_direct` post-push verification (line ~2200): same swap
  for the merge-sha-vs-remote-sha mismatch fallback path that already
  uses `verify_commit_reachable`.

## Tests

`crates/caco-daemon/src/reintegration.rs::tests::`:

- `verify_commit_reachable_polling_succeeds_immediately_when_already_present`:
  asserts the happy path is fast (< 800ms wall) and reaches a commit that
  is already on the remote without polling.
- `verify_commit_reachable_polling_returns_false_after_budget_when_unreachable`:
  bogus SHA, 500ms budget, asserts the helper polls until the budget expires
  (>= 450ms) and then returns false.
- `verify_commit_reachable_polling_picks_up_late_arrivals`:
  spawns a background pusher that pushes the target commit to the remote
  400ms after the helper starts; asserts the helper picks it up within the
  3s budget.

New shared fixture `setup_polling_fixture` builds a minimal bare-remote +
work-checkout pair without depending on the existing larger `setup_repo`.

`VERIFY_POLL_ENV_LOCK` (Mutex) serialises env mutation across the three
tests; lock acquisition uses `unwrap_or_else(|e| e.into_inner())` to
tolerate poisoning from prior panics (matches the established pattern at
seven other lock sites in the same module).

## Verification

- `cargo test -p caco-daemon --lib reintegration` — 112/112 green.
- `cargo test-small` — full small-suite green (197+109+716+277+18+2787+45 = 4149/4149).
- `cargo check -p caco-daemon` — clean.

## Operational impact

- Eliminates the false-failure beat for direct reintegrations where the
  remote ref-cache lags push completion by a few hundred ms.
- Bounded by a 5s default budget so a real push failure still surfaces
  within seconds.
- Env var escape hatch for ops emergencies.
- No change to the happy-path latency: first probe runs immediately and
  early-returns on success.
- Compatible with bd-8b1492's "another agent's fast-forward landed after
  ours" reachability fallback — the polling helper wraps that same check.

## Cross-cutting note

The post-verification flow now has THREE layers of defence:
1. Inner direct merge — push then fetch then sha equality.
2. bd-8b1492 reachability fallback — when the tip differs but our commit
   is still an ancestor.
3. bd-eb8b0f outer verify_direct_outcome — independent re-check from the
   caller's checkout.

bd-6da2c1 makes layer 2 + 3 both polling. Layer 1's sha-equality probe was
intentionally left as a single check because if the equality fails the
fallback layer 2 will then poll.

## Next

Reintegrating direct, closing bd-6da2c1, idling.
