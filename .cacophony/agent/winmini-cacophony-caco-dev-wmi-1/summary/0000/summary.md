# Session summary — bd-20747b Issues 5+6: config sparse converge to security-WHY + empty-string guard

## Goal

Pin two real bugs in the bd-20747b sweep of the new
`caco config sparse show / validate` subcommand (bd-5f6b62 LANDED):
- **Issue 5**: `config sparse show/validate --project bogus` used
  a NEW 7th convention `unknown project: X` instead of the shared
  5-surface security-WHY phrasing. Especially ironic since config
  sparse interacts with the FILESYSTEM CHECKOUT — security framing
  is more germane here than for query-only surfaces.
- **Issue 6**: `--project ''` echoed empty into the error output
  (18th empty-string-bypass surface).

## Bead(s)

- `bd-20747b` — caco config sparse sweep (P3 bug, multi-issue).
  Pins Issues 5+6. Issues 1-4 are POSITIVES (required-flag, 
  --path repeatable, --json includes ok, NEW STRONG-promote `note`
  field for human-readable summary). Issue 7 (path-traversal
  silently accepted in --path) is a security AFFORDANCE GAP
  needing daemon-side spec-validation policy work — defer (the
  paths are accepted today but never escape because no spec is
  defined for these projects). Issue 8 (4th flat-envelope variant)
  is the half-flat envelope cohort — same family as bd-b9eccd that
  wmi-2 just landed.

## Before state

```
$ caco config sparse show --project bogus
error: unknown project: bogus

$ caco config sparse show --project ''
error: unknown project: 
```

## After state

```
$ caco config sparse show --project bogus
error: project 'bogus' is not configured; bead operations must target a configured project to prevent routing to an ambient external board

$ caco config sparse show --project ''
error: --project value cannot be empty for config sparse show

$ caco config sparse validate --project bogus
error: project 'bogus' is not configured; ...

$ caco config sparse validate --project ''
error: --project value cannot be empty for config sparse validate
```

config sparse joins the security-WHY cohort. Updated --project
convention map (12 surfaces, NOW 6 conventions — convention (g)
'unknown project: X' eliminated):
- (a) Security-WHY: 6 surfaces (build, changelog show, project show,
  auto-close-landed, project status [bd-f4957c this segment], config
  sparse show/validate [bd-20747b this segment])
- (b) Truncated 'is not configured': 1 (project show --name)
- (c) Inline-allowed-values: 1 (fleet snapshot --projects)
- (d) Silent-accept: 0 (caco ls/ps fixed by wmi-2 in bd-b9eccd)
- (e) Path-context filesystem: 2 (reconcile-log, snapshot list)
- (f) Defined: list: 1 (validate_project_name internal helper)

Convention (a) now dominates. Drift trend reversing.

## Diff summary

- 1 file changed, +33 / -3 (`crates/caco-cli/src/lib.rs`):
  - dispatcher arms for `config sparse show` + `config sparse validate`
    add upfront empty-string guards for `--project`.
  - `dispatch_config_sparse_show` + `dispatch_config_sparse_validate`
    upgraded the unknown-project error to security-WHY phrasing.

## Validation

- `cargo check -p caco-cli`: clean.

## Operator-takeaway

`caco config sparse show/validate` now joins the security-WHY
cohort (now 6/12 surfaces, dominant convention) and rejects empty
--project upfront (18 empty-string-bypass surfaces patched). The
filesystem-touching surface gets the security framing it deserves.

Push-discipline (post-clarification): own-branch push allowed;
default-branch force-push banned; only reintegrate / complete
land work on main. This session continues to use only local refs
+ daemon-mediated reintegrate.
