# Session summary — bd-7995c8 Issues 4+5: caco fleet --projects empty + --feed-tail leak

## Goal

Pin the two real bugs in the bd-7995c8 fleet sweep:
- **Issue 4**: `caco fleet snapshot --projects ''` silently returned
  an empty snapshot (the 3rd, worst empty-string convention in the
  catalogue). Treat empty as "no filter" matching the most-common
  convention.
- **Issue 5**: `caco fleet snapshot --feed-tail bogus` leaked the
  raw rust `ParseIntError` text (`invalid digit found in string`).
  Replace with the gold-standard `(expected ...)` phrasing.

## Bead(s)

- `bd-7995c8` — caco fleet sweep (P3 bug, multi-issue). This session
  pins Issues 4 and 5. Issues 1-3 are POSITIVES (cohort observations:
  5th cross-namespace shared `--limit/--top` validator, NEW numeric-
  validator gold-standard, NOVEL `value(s):` plural-aware phrasing).
  Issue 6 (`--feed-tail -1` / `--top -1` parser ambiguity, 6th+7th
  surfaces) is the cross-cutting parser concern — **filed as
  bd-02c404 P2 this session** so it has its own pickup signal at 7
  confirmed instances. Issue 7 (silent --feed-tail clamp) is mild.

## Before state

```
$ caco fleet snapshot --projects '' --feed-tail 0
{"agents": {}, "beads": {}, ...}                # SILENT empty result

$ caco fleet snapshot --feed-tail bogus --projects cacophony
error: --feed-tail must be a non-negative integer: invalid digit found in string
                                                # rust internals leak
```

## After state

```
$ caco fleet snapshot --projects '' --feed-tail 0
{... full snapshot of all configured projects ...}
                                                # empty = no filter

$ caco fleet snapshot --feed-tail bogus
error: invalid --feed-tail value 'bogus' (expected a non-negative integer 0..=2000, e.g. 200)
                                                # gold-standard phrasing
```

`--projects` whitespace-only input is also normalised. The
"no filter" semantic matches `caco msg inbox --grep ''` and
`caco event log --command ''`.

## Diff summary

- 1 file changed, +21 / -7 (`crates/caco-cli/src/lib.rs`
  `dispatch_fleet_snapshot`).

## Validation

- `cargo check -p caco-cli`: clean.

## Operator-takeaway

`caco fleet snapshot --projects ''` no longer silently filters to
zero projects — it now defaults to all configured projects.
`--feed-tail bogus` produces a useful actionable error citing the
allowed range and an example, instead of leaking the rust parser's
internal error message. Issue 6 (the `-1` parser ambiguity) is now
its own claimable bead at bd-02c404 P2 — the highest-priority
cross-cutting concern surfaced by this session's CLI sweeps.
