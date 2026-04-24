# Session summary — bd-d761db Issues 5+6: caco notify get exit-code + empty-id

## Goal

Pin the two real bugs in the bd-d761db notify sweep:
- **Issue 5** (NEW): `caco notify get --id bogus --json` returned the
  correct error envelope but **exited 0** instead of 1, breaking
  scripts that gate on exit code (4th-on-shape but only conformer
  failing on exit-code).
- **Issue 6**: empty `--id` was the 6th surface in the empty-string-
  bypass family.

## Bead(s)

- `bd-d761db` — caco notify sweep (P3 bug, multi-issue). This session
  pins Issues 5 and 6. Issues 1-4 are POSITIVES (cohort observations,
  no code change). Issue 7 (bare-`--id` cohort observation) is also
  a positive.

## Before state

```
$ caco notify get --id bogus --json; echo exit:$?
{"ok":false, "error":{"code":"not_found", ...}, "meta":{...}}
exit: 0                                                        # WRONG

$ caco notify get --id ''
error: invalid response (HTTP 404 Not Found): EOF while parsing a value at line 1 column 0
```

## After state

```
$ caco notify get --id bogus --json; echo exit:$?
{"ok":false, "error":{"code":"not_found", ...}, "meta":{...}}
exit: 1                                                        # matches bd/scratch/test show

$ caco notify get --id ''
error: --id cannot be empty (notification IDs must be non-empty strings; see `caco notify list` for available notifications)

$ caco notify get --id '' --json
{
  "ok": false,
  "error": {
    "code": "invalid_argument",
    "message": "--id cannot be empty …"
  }
}
[exit: 1]
```

Exit-code fix is at the dispatch boundary: parse the returned JSON
envelope, set `exit_code = 1` when `ok == false`, `0` otherwise.
This keeps `dispatch_notify_get`'s `Result<String, CliError>`
signature unchanged (no ripple into other callers).

## Diff summary

- 1 file changed, +35 / -1 (`crates/caco-cli/src/lib.rs` notify get
  dispatch arm).

## Validation

- `cargo check -p caco-cli`: clean.

## Operator-takeaway

`caco notify get --id … --json` now exits non-zero on `ok:false`,
matching `caco bd show` / `caco scratch show` / `caco test show`.
Scripts using `if caco notify get --id $id --json …; then`
previously incorrectly entered the success branch on missing IDs;
they now behave correctly. This was a silent bug — no test was
asserting on the exit code.

6th surface to gain an empty-string `--id` guard. The shared
helper bead (bd-29c7e3) filed earlier this session would amortize
the next 4-5.
