# Session summary — Reintegration verification against upstream

## Goal

This session investigated the P0 reintegration work-loss reports and converted the clearest observed failure mode into a concrete fix. The aim was to stop agents from being told their work was stranded when the merge had actually landed upstream but the worker checkout was verifying against a stale managed `origin` view.

## Bead(s)

- `bd-ec1b89` — Investigate work loss in reintegration process

## Before state

- Failing tests: none in scoped validation.
- Relevant metrics: `caco agent audit-reintegration --since 1d` reported no suspicious direct reintegrations, but two unverifiable entries missing persisted outcome data; my prior reintegration had also produced a false post-verification failure before a later fetch showed the merge commit on `origin/main`.
- Context: in managed topology, an agent checkout's `origin` points at the daemon canonical checkout, and the daemon canonical checkout's own `origin` points at the real upstream. Caller-side post-verification in `verify_direct_outcome` fetched and checked `origin/main` from the worker checkout, which can read a stale local branch from the canonical checkout rather than the authoritative upstream.

## After state

- Failing tests: none in scoped validation.
- Relevant metrics: targeted regression `verify_direct_outcome_uses_canonical_upstream_not_stale_worker_origin` passes; `cargo test-small` passes with 252 tests.
- Context: direct reintegration post-verification now detects managed canonical-checkout topology and verifies reachability by fetching the canonical checkout's upstream remote. This preserves the existing polling behavior while avoiding false stranded-work outcomes caused by stale worker-origin refs.

## Diff summary

- Commits: `853f8fd2f`
- Files touched: `crates/caco-daemon/src/reintegration.rs`
- Tests: added 1 regression; no tests removed or ignored.
- Behavioural delta: `verify_direct_outcome` no longer trusts the worker checkout's `origin/main` when that origin is the daemon checkout. It resolves through the canonical checkout to the real upstream and verifies the advertised merge commit there.
- Validation: `cargo test -p caco-daemon verify_direct_outcome_uses_canonical_upstream_not_stale_worker_origin --lib` passed; `cargo test-small` passed with 252 tests.

## Operator-takeaway

The root cause for this class of apparent work loss was a verification-topology bug, not the merge itself disappearing: worker checkouts can see a stale daemon-checkout `origin/main`. The fix makes post-verification authoritative by checking the upstream remote behind the daemon checkout.
