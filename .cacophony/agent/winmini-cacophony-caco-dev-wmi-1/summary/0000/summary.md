# Session summary — bd-89df3d: caco scratch show --note-id '' validator

## Goal

Pin Issue 4 of bd-89df3d: empty `--note-id` previously bypassed
validation, hit a 404 with empty body, and surfaced as a
two-layer-internals leak (HTTP status + serde_json parser error).
Reject empty input up-front with a useful guidance message.

## Bead(s)

- `bd-89df3d` — caco scratch sweep (P3 bug, multi-issue). This
  session pins ONLY Issue 4 (empty-string `--note-id` validator).
  The other items in the bead are positives (Issues 1-3) or
  separate features (Issue 5 — list filter flags), and remain in
  the bead body for future work.

## Before state

```
$ caco scratch show --note-id ''
error: invalid response (HTTP 404 Not Found): EOF while parsing a value at line 1 column 0
```

Two-layer internals leak: HTTP `404` exposed + serde_json parser
error message surfaced as the user-visible "error".

## After state

```
$ caco scratch show --note-id ''
error: --note-id cannot be empty (note IDs must be non-empty strings; see `caco scratch list` for available notes)

$ caco scratch show --note-id '' --json
{
  "ok": false,
  "error": {
    "code": "invalid_argument",
    "message": "--note-id cannot be empty …"
  }
}
[exit: 1]
```

`--json` mode emits the structured envelope on stdout (matching
the bd-87425e fix pattern). Text mode returns a normal `CliError`
with the same message.

## Diff summary

- 1 file changed, +18 / -0 (`crates/caco-cli/src/lib.rs`
  `dispatch_scratch_show`).

## Validation

- `cargo check -p caco-cli`: clean.

## Operator-takeaway

This is the 4th surface to gain an empty-string guard on a required
arg (after bd-3e39a0, bd-b7392e, bd-87425e). A cross-cutting
validator audit (filing as a follow-up bead if not already
captured) would be cheaper than fixing each surface individually.
The bd-89df3d bead remains open with Issue 5 (list filter flags)
unresolved — that's a feature add, not a bug fix.
