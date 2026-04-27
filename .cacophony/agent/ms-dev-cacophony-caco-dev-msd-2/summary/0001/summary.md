## Goal

Fix the pre-commit hook so `git add` after `rustfmt` does not slurp
unstaged hunks from other beads into the commit, breaking atomic bead
commits.

## Bead(s)

- `bd-fd6008` — Pre-commit hook should not stage unrelated unstaged
  hunks.

## Before state

- `.githooks/pre-commit` ran `rustfmt` on the working-tree copy of
  staged `.rs` files, then `git add`-ed them unconditionally. Because
  the working tree may contain unstaged hunks from a different bead,
  those hunks were pulled into the index and committed — operator
  discovered this while splitting bd-dffaba from bd-7c239f.

## After state

- Hook now saves the unstaged diff for staged `.rs` files, resets the
  working tree to match the index (via `git checkout`), runs `rustfmt`
  on the index-only content, `git add`-s the result, then re-applies
  the saved unstaged diff to the working tree only (without staging).
- Tested scenarios:
  1. Staged + unstaged in same file → HEAD has only staged; unstaged
     preserved in working tree.
  2. Staged content needs rustfmt fix, no unstaged → formatted in HEAD.
  3. No `.rs` files staged → hook is a no-op.
  4. Unstaged hunk near a rustfmt-corrected line → HEAD has formatted
     staged content; unstaged hunk (unformatted) preserved.
- If `git apply` fails to restore unstaged changes (extreme edge case),
  the saved patch file is preserved and a warning is printed — the
  commit still succeeds with correct staged content.

## Diff summary

- `.githooks/pre-commit`: rewritten to save/restore unstaged diff
  around the rustfmt + git-add cycle. No new dependencies.

## Operator-takeaway

Atomic bead commits are now safe when unstaged hunks from other beads
exist in the same file. The hook preserves the separation that `git add
-p` or per-hunk staging creates.
