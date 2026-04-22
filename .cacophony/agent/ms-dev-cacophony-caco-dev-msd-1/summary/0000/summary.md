# Session summary — bd-e99303 silent-reconcile false positive in direct reintegrate

## Goal

Close a silent-data-loss path in `caco agent reintegrate --mode direct`:
when the trees-match short-circuit fires inside `finalize_direct_merge`,
declare "reconciled: agent work already on main" only after verifying
the agent's modified files actually match the target branch content.
Never silently drop a fresh agent commit.

## Bead(s)

- `bd-e99303` — Reintegration mis-detects new commits as 'already on main'
  and skips merge silently

## Before state

- Failing tests: none related to this bead.
- Two existing work-loss guards in `finalize_direct_merge`:
  - `bd-c07b08` tree-level check before declaring reconciliation
    (catches stale local agent ref by re-merging from authoritative tip).
  - `bd-f41591` `git cherry`-based unlanded-commit check on the
    *retry-squash* path.
- Gap: the `else` branch where `authoritative_agent_tree_matches_target`
  returns `true` ("Trees match — genuinely reconciled") had no
  defence-in-depth guard. A persistent agent that had reintegrated
  several times in a session could land in a state where the trees
  appeared equal but the agent actually had unlanded patches; the
  flow returned `success: true` and the worker's commit silently
  vanished. Repro recorded in the bead from a real config-helper drop
  on `.cacophony/themes/ultra.yaml`.

## After state

- Failing tests: none.
- New defence in `finalize_direct_merge` trees-match path:
  1. Refresh canonical checkout's local agent-branch ref from the
     authoritative worker checkout (same call already used by the
     bd-f41591 retry path).
  2. Run new `agent_paths_with_unlanded_content` helper, which lists
     every path the agent touched since the merge-base and compares
     blob ids on agent vs `remote/target`.
  3. If any path's content differs (or one side fails to resolve),
     abort the merge, reset the checkout, and return a structured
     conflict outcome with operator-recovery guidance instead of a
     false `reconciled` success.
- Helper deliberately uses per-file blob equality rather than
  `git cherry`. `git cherry` flags every individual agent commit as
  unlanded after a single squash-merge to target (different patch-ids),
  which would break the legitimate `direct_reintegration_push_tags_recovers_on_reconciled_retry`
  reconcile path. Per-file content equality catches genuinely unlanded
  paths in both directions while preserving squash-landed reconciles.
- New unit test
  `agent_paths_with_unlanded_content_distinguishes_landed_vs_pending`
  asserts both invariants: zero unlanded paths after a clean
  squash-merge, and the new file flagged after a fresh agent commit.
- Reintegration test count: 103 → 104, all passing.

## Diff summary

- Files touched: `crates/caco-daemon/src/reintegration.rs`
- Tests: +1 unit test, 0 flipped, 0 removed.
- Behavioural delta: in `finalize_direct_merge` the trees-match
  short-circuit now performs a per-file blob equality check across
  every agent-modified path before reporting `reconciled`. Mismatched
  paths surface as a structured conflict outcome instead of silently
  declaring success.

## Operator-takeaway

The bd-c07b08 + bd-f41591 work-loss guards previously protected the
*retry-squash* branch but the *first-try-trees-match* branch had no
equivalent check. After this change, every reconcile path in
`finalize_direct_merge` either has direct content evidence the work
landed or refuses to declare success. If the trees-match guard ever
fires on legitimately-reconciled work that the per-file check
misclassifies, the failure mode is a loud conflict outcome, not a
silent drop — operators can re-fetch and retry.
