# Session summary — bd-36edaa Issues 3+4: config schema + template-help envelope wrap

## Goal

Pin two real envelope-drift bugs in the bd-36edaa sweep of caco
config diff + schema + template-help:
- **Issue 3**: `caco config schema --json` returned `{ok, sections}`
  — 5th OK+flat envelope drift surface. Wrap to `{ok, data, meta}`.
- **Issue 4**: `caco config template-help --json` returned a flat
  top-level dump `{overview, syntax, builtins, stdlib,
  evaluation_contract}` with NO `ok` field — 4th NO-OK envelope
  drift surface. Wrap to `{ok, data, meta}`.

## Bead(s)

- `bd-36edaa` — caco config diff + schema + template-help sweep
  (P4 bug, multi-issue). Pins Issues 3+4. Issues 1-2 are POSITIVES
  (STRONG-promote NOVEL restart_required + all_match boolean
  computed-summary fields on config diff; gold-standard ✓/!/✗
  glyph-keyed peer-state visualisation). Issue 5 (--node/--peer
  not filtering on config diff) is bd-b76723 affordance gap +
  feature ask — defer (needs design call on filter semantics).

## Before state

```
$ caco config schema --json | jq 'keys'
[
  "ok",
  "sections"
]

$ caco config template-help --json | jq 'keys'
[
  "overview",
  "syntax",
  "builtins",
  "stdlib",
  "evaluation_contract"
]
```

## After state

```
$ caco config schema --json | jq 'keys'
[
  "data",
  "meta",
  "ok"
]

$ caco config schema --json | jq '.data | keys'
[ "sections" ]

$ caco config template-help --json | jq 'keys'
[
  "data",
  "meta",
  "ok"
]

$ caco config template-help --json | jq '.data | keys'
[
  "builtins",
  "evaluation_contract",
  "overview",
  "stdlib",
  "syntax"
]
```

Both surfaces now conform to the canonical `{ok, data, meta}`
envelope. Catalogue update:

- **NO-OK surfaces** before this segment: 4 (cert status, log
  exceptions, mcp, template-help). After: 3 (cert status, log
  exceptions, mcp).
- **OK+flat surfaces** before this segment: 5 (config schema,
  config sparse show, config sparse validate, ...). After: 4
  (config sparse show + validate, ...).

## Diff summary

- 1 file changed, +20 / -7 (`crates/caco-cli/src/lib.rs`):
  - `dispatch_config_schema`: replaced flat `{ok, sections}`
    payload with `{ok, data:{sections}, meta:{section_filter}}`.
  - `dispatch_config_help`: wrapped flat spec dump in `{ok, data,
    meta:{surface}}`.

## Validation

- `cargo check -p caco-cli`: clean.
- No tests referenced these envelopes (greps `crates/caco-cli/tests/`
  for `config schema` / `template-help` / `config_schema_json` —
  zero matches).

## Operator-takeaway

`caco config schema --json` and `caco config template-help --json`
both now expose the canonical `{ok, data, meta}` envelope.
Programmatic CI consumers can reliably check `.ok` and traverse
`.data` across config surfaces. Two more drifters retired this
turn — NO-OK cohort shrinks 4→3, OK+flat cohort shrinks 5→4.

Push-discipline (post-clarification): own-branch push allowed;
default-branch force-push banned; only reintegrate / complete
land work on main. This session continues to use only local refs
+ daemon-mediated reintegrate.
