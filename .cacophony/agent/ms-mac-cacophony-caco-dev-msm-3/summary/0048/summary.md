# Session summary — bd-2f7850 image generate --project validation

## Goal

Fix three test-user-found UX gaps in `caco image generate`:
unvalidated --project, bare 'builder error' leakage, wrong
required-flag check order.

## Bead(s)

- `bd-2f7850` — caco image generate --project validation gaps

## Before state

- `--project nonexistent` cascaded into raw 'builder error' (bypassed
  validating resolve_project via `.or_else` shape)
- Missing --project: --prompt validation fired first (wrong order)
- Bare 'image generation request failed: builder error' \u2014 reqwest
  internal text not user-facing

## After state

- `resolve_project(flags, ...)` called first; rejects unknown projects
  with friendly 'project X is not configured' message (sibling of
  bd-cd2ec9 family)
- Order corrected: project validation now BEFORE --prompt check
- Error path includes `(POST {url})` so operators can spot a bad
  base_url config

## Diff summary

- Commits: 0cfc3eb98b6d
- Files: `crates/caco-cli/src/lib.rs` (+58 -3)
- Tests: +2 (source-level guards in tests mod)

## Operator-takeaway

The `X.or_else(|_| validating_X)` pattern is a footgun \u2014 it skips
validation whenever the non-validating path returns Ok. Worth a sweep
of caco-cli for similar shapes; a quick grep candidate is any
`resolve_*_from_flags_or_env` paired with `.or_else(|_| resolve_*)`.
