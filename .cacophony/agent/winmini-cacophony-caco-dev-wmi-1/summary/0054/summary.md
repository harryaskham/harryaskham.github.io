# Session summary — bd-052aba: --project audit + adopt validate_project_name in choices present

## Goal

Carry out the bd-2a744a follow-up audit: walk the
~30 CLI dispatchers that take `--project` and adopt the
`validate_project_name` helper anywhere a typo would
silently create or tag state for a never-existed project.

## Bead(s)

- `bd-052aba` — own follow-up to bd-2a744a. Closed.

## Audit findings

### Silent-create / silent-tag candidates inspected

- **`caco choices present --project <X>`** — vulnerable.
  `dispatch_choices_present` accepts `project: &str` and
  POSTs straight to `/api/v1/choices/present`. Daemon
  stores the string verbatim; a typo like
  `--project cacaphony` would create a choice tagged
  with the wrong project, never reachable from
  `caco choices current --project cacophony`.  **Fixed**
  with a `validate_project_name(project, co)?` pre-check
  at the call site (lib.rs:9794).
- **`caco choices reissue --choice-id <X>`** — not
  vulnerable; takes a choice_id, no `--project` flag.
- **`caco choices resolve --choice-id <X>`** — same as
  reissue; lookup by id, daemon validates id existence.

### Not silent-create (already validated downstream)

- `caco bd *` family (`bd list/show/create/update/...`)
  hits the daemon beads endpoint which returns an
  `unknown project` error from the daemon proxy.
- `caco notes set/unset` — does not exist (greps
  returned no `dispatch_notes*` symbol).
- `caco pin` / `caco changelog set` — also not present
  as dispatchers.
- `caco config sparse show --project X` — already
  hard-errors via the existing `unknown project: X`
  pattern (one of the inspirations for bd-2a744a).

### Pattern: `--project` flag with daemon-side validate

Most dispatchers fall into this category. They forward
the project name to a daemon endpoint that already
returns a structured error if unknown. No client-side
pre-validation needed; the daemon is the source of
truth.

## Before state

- `caco choices present --project cacaphony --agent-id X
   --choices '[{...}]'` would succeed and create a
  choice-row tagged with project='cacaphony'. The choice
  would be invisible to anyone querying with the correct
  project name.

## After state

- The same invocation now hard-errors:
  `unknown project: cacaphony. Defined: cacophony, ...`

## Diff summary

- 1 file touched, +5 / 0:
  - `crates/caco-cli/src/lib.rs`: added one
    `validate_project_name(project, co)?` line in the
    `choices present` arm of `dispatch`.

## Verification

- `cargo build -p caco-cli`: clean.
- Audit grep confirmed the absence of `dispatch_notes*`,
  `dispatch_pin*`, `dispatch_changelog_set` symbols.

## Operator-takeaway

bd-2a744a's `validate_project_name` helper now also
guards `caco choices present`. The audit found one more
silent-tag site beyond mode {show,set,clear}; the rest
of the `--project` dispatchers either don't create state
on the daemon or are already daemon-validated. Helper
remains available for future dispatchers; future
silent-create sites should adopt the same one-liner.
