# Session summary — bd-4df284 Issues 5+6: scratch list --limit upfront validation

## Goal

Pin two related bugs in the bd-4df284 scratch+service sweep:
- **Issue 5**: `caco scratch list --limit bogus` surfaced the 2nd
  HTTP-400-JSON-leak (daemon rejected verbatim string with a 400 +
  non-JSON body, surfacing as 'invalid response (HTTP 400 Bad
  Request): expected value at line 1 column 1'). Joins bd-dda312
  changelog show in the HTTP-400-JSON-leak class.
- **Issue 6**: `caco scratch list --limit 0` silently succeeded
  with 0 results, drifting from the USAGE-GUIDANCE template now
  shared by bd-1c1d0f / bd-0b47a7 / bd-2dae3c.

Both fixed with upfront client-side validation using the canonical
USAGE-GUIDANCE phrasing.

## Bead(s)

- `bd-4df284` — caco scratch + service sweep (P3 bug, multi-issue).
  Pins Issues 5+6. Issues 1-3 are POSITIVES (NOVEL STRONG-promote
  error-as-card rendering on scratch show; 3 sharp scratch
  envelopes; 6th 'Configured: ...' surface). Issue 4 (scratch show
  --note-id '' HTTP 404 EOF leak) is ALREADY FIXED — bd-89df3d
  this session shipped the upfront empty-string guard with the
  exact same 'two-layer internals leak' commentary in the source.
  Issue 7 (--limit -1, 19th parser-ambiguity) covered by bd-02c404.
  Issue 8 (service show/status --json severely underspecified)
  needs daemon-side data plumbing — defer (operator clarified
  service is sole-owner caco-ctrl@helsinki). Issue 9 (--node
  bd-b76723 affordance gap) belongs to bd-b76723 family epic.

## Before state

```
$ caco scratch list --limit bogus
error: invalid response (HTTP 400 Bad Request): expected value at line 1 column 1

$ caco scratch list --limit 0
caco scratch list — 0 note(s)
```

## After state

```
$ caco scratch list --limit bogus
error: invalid --limit value 'bogus' (expected a positive integer, e.g. 50)

$ caco scratch list --limit 0
error: --limit must be >= 1 (use --limit 1 for the most recent note, or omit --limit for the default)
```

Joins the USAGE-GUIDANCE cohort (bd-1c1d0f / bd-0b47a7 /
bd-5f81fe / bd-2dae3c) and breaks the HTTP-400-JSON-leak path
(was 2 surfaces — now 1, with bd-dda312 changelog show still
outstanding).

## Diff summary

- 1 file changed, +21 / -0 (`crates/caco-cli/src/lib.rs`):
  - `dispatch_scratch_list`: parse `--limit` upfront, reject
    non-numeric and zero with canonical USAGE-GUIDANCE phrasing.

## Validation

- `cargo check -p caco-cli`: clean.

## Operator-takeaway

`caco scratch list --limit X` now validates client-side with the
canonical USAGE-GUIDANCE phrasing — no more mysterious HTTP 400
JSON-parser leaks, no more silent 0-result drift. The scratch
namespace now joins the gold-standard validator cohort that
already covers bd graph / event log / auto-close-landed / triage.

Push-discipline (post-clarification): own-branch push allowed;
default-branch force-push banned; only reintegrate / complete
land work on main. This session continues to use only local refs
+ daemon-mediated reintegrate.
