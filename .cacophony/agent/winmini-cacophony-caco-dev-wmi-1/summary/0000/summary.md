# Session summary — bd-02c5f7: caco branch-parent --json no-subcommand envelope

## Goal

Stop the help-dispatcher from leaking through as a fake `{command,
summary, args, subcommands}` data response when a branch parent
(e.g. `caco config sparse`) is invoked bare with `--json`. Replace
with the standard `{ok:false, error:{...}}` envelope.

## Bead(s)

- `bd-02c5f7` — caco config sparse --json (parent without
  subcommand) emits COMMAND-HELP JSON as fake response — novel
  envelope = help-dispatcher leak (P3 bug, multi-issue).
  This session pins ONLY Issue 1. Issues 2-3 (sparse show/validate
  ignore --json on errors; sparse show --project bogus missing
  inlined available projects) are siblings of bd-87425e / bd-89df3d
  and remain in the bead body for follow-up.

## Before state

```
$ caco config sparse --project bogus --json
{
  "command": "caco config sparse",
  "summary": "Inspect project-level sparse-checkout spec (bd-5f6b62).",
  "args": [],
  "subcommands": [
    {"name": "show", ...},
    {"name": "validate", ...}
  ]
}
[exit: 0]
```

A NOVEL envelope shape (no `ok`, no `error`, no `data`) returned
with exit 0. Scripts piping to `jq` got fields they didn't expect
and no signal that no work happened.

## After state

```
$ caco config sparse --json
{
  "ok": false,
  "error": {
    "code": "no_subcommand",
    "message": "`caco config sparse` requires a subcommand: show, validate",
    "available": ["show", "validate"]
  }
}
[exit: 2]
```

Standard error envelope on stdout, with `available` carrying the
subcommand list (preserving the discoverability that the help-leak
shape provided). Exit 2 (matches the unknown-subcommand path
right above it).

Text mode is unchanged: bare `caco config sparse` still renders
the human-readable help page.

## Diff summary

- 1 file changed, +30 / -0 (`crates/caco-cli/src/lib.rs` catch-all
  branch in the dispatcher's `_ =>` arm).

## Validation

- `cargo check -p caco-cli`: clean.

## Operator-takeaway

This is a **cluster-wide** fix, not just for `caco config sparse`.
Every branch parent that lacks an explicit dispatch case (the vast
majority — `caco bd`, `caco scratch`, `caco config`, etc.) now
returns the standard error envelope on bare `--json` invocation
instead of leaking the help structure.

Three branch-roots (`caco tui --json`, `caco daemon --json`,
`caco tts daemon --json`) still emit help-as-JSON because SPEC 8.2
explicitly mandates that — those have an interactive default action
and `--json` returns help to prevent accidental TTY-grab. Out of
scope for this bead.

The `available` field is new — consumers that previously relied on
the `subcommands` field of the help-leak shape can now read
`error.available` for the same data in a normalized format.
