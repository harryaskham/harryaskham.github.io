# Session summary — bd-5e10e5: summary JSON error envelope + ls JSON wrap

## Goal

Tighten two read-side CLI JSON drifts from the bd-5e10e5 test-user
pass:

1. `caco summary --json` with missing `--since` leaked a text-mode
   error (`--since is required for summary`) instead of a structured
   JSON error envelope.
2. `caco ls --json` still emitted an OK+flat shape
   `{ok, entries, count, runtime_root, node}` instead of the
   canonical `{ok, data, meta}` envelope.

These are small, contained read-side convergence fixes that keep
programmatic consumers from special-casing these surfaces.

## Bead(s)

- `bd-5e10e5` — caco summary + ls + status pass; pinning the two real
  drifts (`summary --json` text leak, `ls --json` OK+flat).
- Promote candidates in the bead body remain untouched:
  `value cannot be empty`, inline format examples, 4-element
  USAGE-GUIDANCE, `Valid kinds:`, security-WHY, strong status JSON.

## Before state

- `caco summary --json` (no `--since`) returned plain text on stdout:
  - `error: --since is required for summary`
- `caco ls --json` returned:
  - `{ ok, entries, count, runtime_root, node }`
  - metadata fields were flattened at top level instead of living under
    `meta`.

## After state

- `caco summary --json` (no `--since`) now returns structured JSON:
  - `{ ok:false, error:{ code:"missing_argument", message:"--since is required for summary" }, meta:{ surface:"caco summary", required:["--since"] } }`
- `caco ls --json` now returns canonical wrapped JSON:
  - `{ ok:true, data:{ entries:[...] }, meta:{ count, runtime_root, node } }`

## Diff summary

- Files touched:
  - `crates/caco-cli/src/lib.rs`
  - `.cacophony/agent/winmini-cacophony-caco-dev-wmi-1/summary/0000/summary.md`
- Code changes:
  - top-level `summary` dispatch arm now emits a structured JSON error
    envelope for missing `--since` when `--json` is requested, while
    leaving text mode unchanged.
  - `dispatch_ls()` now wraps JSON output in canonical
    `{ok,data,meta}` shape, moving `count`, `runtime_root`, and `node`
    into `meta` and placing `entries` under `data.entries`.
  - added source-grep tests pinning both contracts.

## Embedded artefacts

- none

## Operator-takeaway

This was a clean read-side polish bead: one text leak removed, one flat
JSON drift removed. `caco summary` and `caco ls` are now easier to use
from scripts without ad hoc parsing or per-command envelope shims.
