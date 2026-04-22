# Session summary — bd-6f0787: service logs --lines / msg inbox --offset validation; new validate_non_negative_int helper

## Goal

Probe sweep finds two more flag-shape leaks where
non-numeric input bypasses caco's CLI layer and
surfaces the underlying tool's parse-failure shape:

- `caco service logs --lines abc` → "Failed to add
  match 'abc': Invalid argument" (journalctl).
- `caco msg inbox --offset abc` → "daemon response
  parse failed (HTTP 400 Bad Request) ..." (axum).

Different shape from the bd-a08f85 family because 0
is legitimately allowed (offset 0 = start at
beginning; --lines 0 is journalctl's documented "no
entries" response).  Needs a sibling helper.

## Bead(s)

- `bd-6f0787` — own follow-up. Closed.

## Before state

```
$ caco service logs --lines abc
Failed to add match 'abc': Invalid argument

$ caco msg inbox --project cacophony --offset abc
error: daemon response parse failed (HTTP 400 Bad Request):
  expected value at line 1 column 1
```

## After state

```
$ caco service logs --lines abc
error: invalid --lines value: abc (expected a
non-negative integer)

$ caco msg inbox --project cacophony --offset abc
error: invalid --offset value: abc (expected a
non-negative integer)

$ caco msg inbox --project cacophony --offset 0
(legit; returns first page)

$ caco service logs --lines 5
(legit; returns last 5 entries)
```

## Diff summary

- 1 file touched, +20 / −0:
  - `crates/caco-cli/src/lib.rs`:
    - new `validate_non_negative_int(flag_name, value)
       -> Result<usize, CliError>` helper
      immediately after `validate_positive_limit`.
      Accepts 0 (unlike the positive sibling).
    - service logs dispatch arm validates `--lines`.
    - msg inbox dispatch arm validates `--offset`
      (in addition to existing --limit and --tail).

## Verification

- `cargo build --bin caco`: clean.
- All 4 cases in the After state above verified.

## Operator-takeaway

4 helpers now in the validation cluster:
- `validate_enum_flag(flag, value, allowed)` (bd-2c6856)
- `validate_since_or_rfc3339(flag, value)` (bd-93e389)
- `validate_positive_limit(flag, value)` (bd-a08f85)
- `validate_non_negative_int(flag, value)` (this bead)

Pattern of helper splits is now stable: the
non-negative variant covers offset/lines-style flags
where 0 has a documented meaning; the positive
variant covers limit/count-style flags where 0 is
nonsense.

Probe leftover: `caco bd list --offset` doesn't exist
(would be useful for paging — separate feature
request, not a typo bug).  `caco event log --offset`
likewise doesn't exist.  Both could be future
features but aren't honesty bugs today.

The pattern of "leak underlying tool error" is
more general than just numeric typos — for any flag
that's forwarded raw to a downstream tool (journalctl,
daemon, git), the dispatch boundary should validate
shape so the operator gets a CLI-level error, not a
tool-level one.  Worth one more sweep pass next round.
