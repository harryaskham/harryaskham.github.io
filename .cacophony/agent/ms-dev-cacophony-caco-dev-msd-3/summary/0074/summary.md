# Session summary — router quick-file overlap splitting

## Goal

Reduce duplicate implementation risk from broad quick-file/operator request bursts by making the router profile explicitly split overlapping beads into non-overlapping child slices and mark broad aggregate beads as coordination parents.

## Bead(s)

- `bd-03e633` — Reduce overlapping quick-file beads for chat composer autocomplete slices

## Before state

- Failing tests: none known for this doc/profile-only slice.
- Relevant metrics: recent chat composer work required manual coordination across `bd-ce45b0`, `bd-64e7b4`, and `bd-f42253` because broad autocomplete/routing requests overlapped.
- Context: the router profile already handled duplicate checks and post-creation refinement, but did not explicitly tell routers how to split broad quick-file bursts into child slices or block aggregate beads on those children.

## After state

- Failing tests: none.
- Relevant metrics: router instructions now contain a dedicated quick-file overlap section; top-level operator docs now mention router burst dedup/splitting.
- Context: future router-filed bursts should compare recent related beads, create/update non-overlapping child slices, declare sibling out-of-scope boundaries, and use scratch memory so follow-up refinements target the same burst.

## Diff summary

- Commits: `8d55c69dc5`
- Files touched: `.cacophony/profiles/router.md`, `AGENTS.md`, `README.md`
- Tests: no Rust tests added; validated after rebase with `git diff --check origin/main...HEAD` and targeted `rg` checks for the new guidance.
- Behavioural delta: router prompt guidance now says to split broad quick-file bursts, mark aggregate beads as coordination parents blocked on children, coordinate existing overlapping active owners before filing more work, and persist burst state for follow-up refinement.

## Operator-takeaway

This is a prompt/documentation guardrail rather than daemon logic: it should reduce the chance that future quick-file bursts dispatch overlapping workers on the same UI surface, but already-running router agents may need recreation/profile refresh before they inherit the new guidance.
