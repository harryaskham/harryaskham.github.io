# Session summary — bd-6dc352: build show empty-id validator + project error lists configured

## Goal

Address the two implementable bugs in bd-6dc352's
mostly-positive conformance survey of `caco build`:

- **Issue 9** — `caco build show --id ''` leaks an HTTP 404
  EOF parser error instead of validating empty-string upfront
  (9th member of the empty-string-bypass cohort).
- **Issue 5 improvement** — promote the gold-standard
  `--project` security-WHY error by also listing configured
  projects (currently only explains the WHY, doesn't give
  the corrective info).

The remaining issues (1–7 are POSITIVE conformance
observations; Issue 8 `--limit -1` is parser-level
cross-cutting) are out of scope.

## Bead(s)

- `bd-6dc352` — `caco build STRONGLY CONFORMANT — ... 8th
  parser-ambiguity --limit -1; 9th empty-string-bypass`.

## Before state

- `caco build show --id ''` →
  `error: invalid response (HTTP 404 Not Found): EOF while
  parsing a value at line 1 column 0` (leaked HTTP plumbing).
- `caco build list --project bogus` →
  `error: project 'bogus' is not configured; bead operations
  must target a configured project to prevent routing to an
  ambient external board` (no list of valid projects).

## After state

- `caco build show --id ''` →
  `error: --id must not be empty for caco build show`
  (gold-standard 'must not be empty for caco X' template
  established by msg snapshot --agent).
- `caco build list --project bogus` (and any other
  resolve_project / resolve_projects callsite) →
  `error: project 'bogus' is not configured; bead
  operations must target a configured project to prevent
  routing to an ambient external board. Configured:
  cacophony, picasso-health, ...` (security-WHY *plus*
  corrective info).
- The `Configured: ...` suffix lands on all three callsites
  (resolve_project --project arm, resolve_project
  CACO_PROJECT arm, resolve_projects fan-out arm).

## Diff summary

- `crates/caco-cli/src/lib.rs`:
  - `dispatch_build_show`: empty-string guard on `--id` after
    the required-flag check, with the gold-standard wording.
  - `resolve_project`: `--project` flag arm + `CACO_PROJECT`
    env arm both gain the `Configured: {names}` (or `(none)`)
    suffix.
  - `resolve_projects` (the fan-out variant used by
    snapshot/list-style multi-project commands): same
    `Configured: ...` suffix.
  - 2 new tests:
    - `dispatch_build_show_rejects_empty_id` — source-greps
      the dispatcher body for the gold-standard wording.
    - `resolve_project_error_lists_configured_projects` —
      source-greps the file for at least 3 occurrences of the
      'project to prevent routing ... Configured:' compound
      suffix so the three callsites can't drift independently.
- `cargo test -p caco-cli --lib -- ...`: both pass.
- `cargo test-small`: 176 pass.

## Operator-takeaway

The `--project` change is cross-cutting: every command path
that calls `resolve_project` / `resolve_projects` now emits
the richer error wording. That's most read-side surfaces
(`build`, `test`, `release`, `bd`, `image`, `summary`,
`changelog`, etc.) — operators should see the
`Configured: ...` suffix consistently across the cluster
after this lands.

The gold-standard wording is now:

```
project 'X' is not configured; bead operations must target
a configured project to prevent routing to an ambient
external board. Configured: a, b, c
```

This combines all three properties the bead family has been
calling out separately:
1. Names the bad value.
2. Explains the WHY (security rationale).
3. Inlines the allowed alternatives.

Out of scope:
- **Issue 8** (`--limit -1` → 'unsupported flag: -1') is a
  parser-level ambiguity affecting 8 surfaces; needs the
  flag-parser to recognise negative-integer values rather
  than treating them as additional flag tokens. Worth a
  dedicated bead.
- The other 7 issues in bd-6dc352 are POSITIVE conformance
  observations (12th inline-allowed-values exemplar, 5th
  JSON-error-envelope exemplar, etc.) — no action needed,
  they're meta-tracker updates for bd-5ae1ce.
