# Session summary — CLI @-target error wording

## Goal

Improve the error message when `caco @all` (or `caco @node1,node2`)
is invoked without a command.

## Bead(s)

- `bd-6cf7eb` — [CLI polish] at-all missing-command discoverability

## Before state

- Error: "no command specified after @-target; usage: caco @all <command>"

## After state

- Error: actionable message pointing to `caco --help` with concrete
  examples (`caco @all status`, `caco @node1,node2 config validate`).

## Diff summary

- 1 file, +3/-1 in `crates/caco-cli/src/lib.rs`.

## Operator-takeaway

Tiny UX polish — error messages should teach, not just report.
