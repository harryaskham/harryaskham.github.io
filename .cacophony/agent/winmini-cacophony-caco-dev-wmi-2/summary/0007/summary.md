# Session summary — bd-d492dd: caco sidecar list accepts --service (sister-symmetry with status)

## Goal

Eliminate the sister-surface asymmetry where `caco sidecar
status --service X` was the gold-standard inline-allowed-values
exemplar but `caco sidecar list --service X` fired bd-b76723
'unrecognised flag' and silently returned the unfiltered list.

## Bead(s)

- `bd-d492dd` — promotes `sidecar status --service` as the
  SECOND gold-standard inline-allowed-values exemplar
  (alongside `caco profile show --name`); flags Issue 2
  (--service asymmetry status vs list); Issues 3+4 are
  cross-cutting / positive observations.

## Before state

- `caco sidecar status --service bogus` →
  `error: unknown service 'bogus' for node 'helsinki'.
  Configured: caco-daemon, caco-tts-daemon (or omit
  --service for all)` — gold-standard.
- `caco sidecar list --service bogus` →
  `warning: bd-b76723: caco sidecar list received
  unrecognised flag(s): --service. ...` then unfiltered
  output. Operator who learned `--service` from `status`
  can't transfer the muscle-memory to `list`.

## After state

- New `SIDECAR_LIST_ARGS = [--service]` ArgSpec consts.
- `sidecar list` `CommandSpec` upgraded from `mcp_leaf` to
  full struct so `args` surfaces in `--help` and bd-b76723
  detection accepts the flag.
- `dispatch_sidecar_list` takes `filter_service: Option<&str>`
  and validates against `mgr.configured_services()` using the
  exact gold-standard wording from `sidecar status`:
  `unknown service 'X' for node 'Y'. Configured: ... (or
  omit --service for all)`.
- Both text and JSON outputs honour the filter (single-element
  vec when set, full configured list otherwise).

## Diff summary

- `crates/caco-cli/src/lib.rs`:
  - Added `SIDECAR_LIST_ARGS` const next to `SIDECAR_STATUS_ARGS`.
  - `sidecar list` `CommandSpec`: full struct with
    `args: SIDECAR_LIST_ARGS` (was `mcp_leaf` shorthand).
  - `sidecar list` dispatch arm: extracts `--service` and
    forwards to dispatcher.
  - `dispatch_sidecar_list`: new `filter_service` param,
    validator using sidecar-status gold-standard wording,
    services vec narrowed when filter is active.
  - 2 new tests:
    - `sidecar_list_advertises_service_filter_in_help` — runs
      `--help --json` and asserts `--service` appears in args.
    - `dispatch_sidecar_list_validator_mirrors_status_wording`
      — source-greps the dispatcher body for the three
      gold-standard wording substrings (`"unknown service '"`,
      `"Configured:"`, `"(or omit --service for all)"`) so the
      sister-surface symmetry can't drift in future refactors.
- `cargo test -p caco-cli --lib -- ...`: both pass.
- `cargo test-small`: 162 pass.

## Operator-takeaway

Issues 1 (positive promotion of sidecar status --service
gold-standard), 3 (--node gap, bd-5ae1ce family, 6th
instance), and 4 (positive sister-symmetric envelope
observation for caco sidecar) are noted in the bead but
out of scope here — they're cross-cutting or
informational.

The implementable Issue 2 is what this fix addresses. The
template that landed (`unknown X 'Y' for scope Z. Allowed:
... (or omit --X for all)`) is now used identically across
`sidecar status` and `sidecar list`. This is the second
sister-pair to share validator wording verbatim — the first
was bd-53e157 (`node show` vs `node status`). Worth
generalising into a small validator helper if a third pair
appears.

The `mcp_leaf` → full `CommandSpec` upgrade pattern (also
recently used on `caco update status` for bd-32a91d) is the
canonical way to add an arg to a previously argument-less
subcommand without losing MCP enablement / agent-safety
defaults.
