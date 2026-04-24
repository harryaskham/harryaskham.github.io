# Session summary — bd-851658 Issue 4: caco operator-actions list self-contradicting --limit fix

## Goal

Pin Issue 4 of the bd-851658 release+operator-actions sweep: the
top-level `caco operator-actions list` was declared as a
no-args leaf, so the dispatcher fired the bd-b76723 unrecognised-
flag warning on `--limit`/`--project`/`--max-age`/`--include-
closed` even though the underlying handler honours all four.
Operator saw BOTH the warning AND a downstream validator error
from the same flag (self-contradicting).

## Bead(s)

- `bd-851658` — caco release + operator-actions sweep (P4 bug,
  multi-issue). Pins Issue 4. Issues 1-3, 5-6 are POSITIVES /
  CORRECTIONs / cohort observations. Issue 7 (release status --id ''
  HTTP 404 EOF leak) is the 11th empty-string-bypass — covered by
  bd-29c7e3 (cross-cutting `validate_non_empty_id` helper meta-
  bead). Issue 8 (--limit -1 parser ambiguity) is covered by
  bd-02c404 (cross-cutting parser meta-bead filed earlier this
  session).

## Before state

```
$ caco operator-actions list --limit 0
warning: bd-b76723: `caco operator-actions list` received unrecognised flag(s): --limit. These were ignored by the dispatcher.
error: --limit must be >= 1 (omit --limit for the default of 200)
```

The dispatcher claimed `--limit` was "ignored", and then the
downstream validator fired anyway. Mind-bending mixed signal.

## After state

```
$ caco operator-actions list --help
  --project          Project name (default: first configured or CACOPHONY_PROJECT).
  --max-age          Hide beads older than this (e.g. 24h, 7d). Default: show all.
  --limit            Maximum number of beads to return.
  --include-closed   Also show recently-closed operator-action beads (default: open only).

$ caco operator-actions list --limit 0
error: --limit must be >= 1 (omit --limit for the default of 200)
```

bd-b76723 warning is gone. Validator still fires (correct). The
top-level `operator-actions list` and `caco bd operator-actions`
now have identical declared flags AND identical runtime behaviour
(both already routed through `dispatch_bd_operator_actions`).

## Diff summary

- 1 file changed, +14 / -2 (`crates/caco-cli/src/lib.rs`):
  - Top-level `operator-actions list` leaf promoted to explicit
    CommandSpec, reusing the existing `BD_OPERATOR_ACTIONS_ARGS`.
  - No handler change (already accepted these flags).

## Validation

- `cargo check -p caco-cli`: clean.

## Operator-takeaway

`caco operator-actions list --limit N` Just Works without the
spurious warn-then-error pairing. The flag was always honoured by
the handler; only the dispatcher's args spec was missing.

Same family as bd-c0c8b3 (`caco bd operator-actions
--include-closed` inverted) — both are dispatcher/handler args-
spec sync issues. This one was the simpler hoist; the inversion
case is a separate fix.

Push-discipline (post-clarification): own-branch push allowed;
default-branch force-push banned; only reintegrate / complete
land work on main. This session continues to use only local refs
+ daemon-mediated reintegrate.
