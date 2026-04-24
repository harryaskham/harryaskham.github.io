# Session summary — bd-684112 Issues 4+5+7: caco doctor --top phrasing + schema --json envelope

## Goal

Pin three real bugs in the bd-684112 doctor sweep:
- **Issue 4**: `caco doctor --top 0` had different parenthesized
  hint phrasing from the 7-surface shared validator. Align (with
  doctor's top-N semantics noted in the hint).
- **Issue 5**: `caco doctor --top bogus` leaked the raw rust
  ParseIntError ('invalid digit found in string'). Use the
  gold-standard 'invalid --X value Y (expected ...)' phrasing.
- **Issue 7**: `caco doctor schema --json` returned a flat
  `{databases, summary}` shape with no `ok`. Wrap in standard
  `{ok, data, meta}` envelope.

## Bead(s)

- `bd-684112` — caco action + doctor sweep (P3 bug, multi-issue).
  Pins Issues 4, 5, 7. Issues 1-2 are POSITIVES (action run --json
  6th JSON-error-envelope exemplar; NEW required-flag-with-usage-
  example pattern). Issue 3 is the 10th empty-string-bypass — same
  family as bd-29c7e3 (shared `validate_non_empty_id` helper meta-
  bead) — leaving for the cross-cutting fix. Issue 6 (--top -1
  parser ambiguity) is the 10th surface of the parser bug filed
  this session as bd-02c404 P2. The OPERATIONAL signal (helsinki
  UNHEALTHY config-hash mismatch all 6 peers + astra unreachable)
  belongs to cluster-ctrl, not caco-cli.

## Before state

```
$ caco doctor --top 0
error: --top must be >= 1 (use no --top for full output)

$ caco doctor --top bogus
error: --top must be a positive integer: invalid digit found in string

$ caco doctor schema --json | jq 'keys'
["databases", "summary"]
```

## After state

```
$ caco doctor --top 0
error: --top must be >= 1 (use --top 1 for the single highest-severity check, or omit --top for the full output)

$ caco doctor --top bogus
error: invalid --top value 'bogus' (expected a positive integer, e.g. 5)

$ caco doctor schema --json | jq 'keys'
["data", "meta", "ok"]
$ caco doctor schema --json | jq -e .ok
true
```

## Diff summary

- 1 file changed, +30 / -8 (`crates/caco-cli/src/lib.rs`):
  - `--top 0` and `--top bogus` error wording in the doctor
    dispatcher (2 messages).
  - `dispatch_doctor_schema` JSON branch wraps in standard envelope.
  - 2 existing tests updated for the new envelope path.

## Validation

- `cargo check -p caco-cli`: clean.
- `cargo test -p caco-cli --lib doctor_schema`: 5/5 pass.

## Operator-takeaway

Doctor `--top` errors now match the rest of the cluster's numeric-
validator phrasing (gold-standard since bd-c061d4 / bd-7995c8).
`caco doctor schema --json` joins the standard envelope cohort,
shrinking the no-ok surface count from 6 → 5 (`cert status`,
`event log`, `log exceptions`, `mcp`, `caco summary` remain).

**BREAKING for scripts** reading top-level `.databases` / `.summary`
from `caco doctor schema --json`:
- `.databases` → `.data.databases`
- `.summary.total_drift_columns` → `.meta.total_drift_columns`
- `.summary.missing_tables` → `.meta.missing_tables`
- `.ok` is now present (always `true`).

Push-discipline (post-clarification): own-branch push is allowed,
default-branch force-push is banned, only `caco agent reintegrate`
/ `complete` move work onto main. This session has only ever used
local refs + daemon-mediated reintegrate.
