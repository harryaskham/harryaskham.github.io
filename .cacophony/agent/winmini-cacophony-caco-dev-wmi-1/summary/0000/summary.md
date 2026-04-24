# Session summary — bd-754fde Issue 5: caco test show/logs --id '' validator

## Goal

Pin Issue 5 of the bd-754fde sweep: empty `--id` on `caco test show`
and `caco test logs` previously bypassed validation, hit `/tests/`
with a trailing-slash 404, and surfaced as a serde_json EOF parser
error (sixth surface in the empty-string-bypass family). Reject
empty up-front with the same guard pattern landed for bd-89df3d.

## Bead(s)

- `bd-754fde` — caco test sweep (P3 bug, multi-issue). Pins Issue 5
  (empty `--id` validator) only. Issues 1-4 are POSITIVES (cohort
  observations, no code change); Issue 6 (`--limit -1` parser
  ambiguity) is the same cluster-wide parser concern as bd-9c55aa
  Issue 5 and remains in the bead body for a parser-level fix.

## Before state

```
$ caco test show --id ''
error: invalid response (HTTP 404 Not Found): EOF while parsing a value at line 1 column 0
```

Two-layer internals leak: HTTP `404` + serde_json parser error
surfaced as the user-visible "error". `caco test logs --id ''`
exhibited the identical defect.

## After state

```
$ caco test show --id ''
error: --id cannot be empty (test job IDs must be non-empty strings; see `caco test list` for available jobs)

$ caco test show --id '' --json
{
  "ok": false,
  "error": {
    "code": "invalid_argument",
    "message": "--id cannot be empty (test job IDs must be non-empty strings; see `caco test list` for available jobs)"
  }
}
[exit: 1]
```

Same guard applied to `caco test logs` (sister surface within the
test namespace).

## Diff summary

- 1 file changed, +30 / -0 (`crates/caco-cli/src/lib.rs` —
  `dispatch_test_show` and `dispatch_test_logs`).

## Validation

- `cargo check -p caco-cli`: clean.

## Operator-takeaway

This is the **5th surface** to gain an empty-string guard
(bd-3e39a0, bd-b7392e, bd-87425e, bd-89df3d, this bead). The
cross-cutting validator audit is increasingly justified — at this
rate every surface with a required ID flag will need the same
guard individually. A shared `validate_non_empty_required_id`
helper (or a dispatcher-level "required & non-empty" ArgSpec hook)
would let us pin the whole class at once.
