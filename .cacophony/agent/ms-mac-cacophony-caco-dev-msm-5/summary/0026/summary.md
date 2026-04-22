# Session summary 0026 — dev.md: force-with-lease last-resort doc

## Goal

Document the narrow recovery path used in summary 0025 so future
sessions don't repeat the cherry-pick conflict loop.

## Bead(s)

- self-improvement (no bead claim).

## Before state

- dev.md described the standard FF-recovery loop only.
  When `caco agent reintegrate` had internally resolved a
  conflict against a concurrent main update (bd-7858a4),
  the cherry-pick recovery re-conflicted in a loop because the
  origin agent branch lacked the upstream context.

## After state

- dev.md gains a "Last-resort: when concurrent main moves cause
  unresolvable divergence (bd-d8fc57)" section under the
  non-fast-forward footgun.
- Documents `git push origin --force-with-lease HEAD:agent/<branch>`
  as a one-shot recovery, scoped to the agent branch only,
  and emphasises documenting use in the session summary.

## Diff summary

- Commit: `d160ce34`.
- Files (1): `.cacophony/profiles/dev.md`.

## Operator-takeaway

If you exhaust the cherry-pick FF-recovery loop after caco's
internal rebase merged a conflict, push the agent branch with
`--force-with-lease` and retry reintegrate. Never apply this to
main. Always note the use in the session summary so the pattern
stays surfaceable.
