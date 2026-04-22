# Session summary 0017 — bd-2e2338 slice 1: caco bd triage --next

## Goal

Ship the foundational read-only primitive `caco bd triage --next` so
operators can quickly see the head of the 988-draft backlog without
loading TUI / web. The interactive promote/discard/merge-into flow
(the bigger UX scope of the parent bead) is filed as a follow-up.

## Bead(s)

- `bd-2e2338` — primary; this delivers the `--next` primitive.
- Filed `bd-eef036` for the interactive triage loop.

## Before state

- `caco bd list --status draft` works but returns the entire 988-row
  draft pool with default sort (creation order ascending = oldest
  first by accident, but no explicit semantics around triage).
- No focused subcommand for the "process next draft" use case.
- Operators authoring a draft-triage script had to re-derive the
  filter/sort flags every time.

## After state

- New `caco bd triage --next [--limit N] [--type T] [--priority P]
  [--json]` subcommand.
- Reuses the existing `/api/v1/projects/<p>/beads` endpoint with
  `status=draft&sort=created` (oldest-first by explicit semantic).
- Default `--limit 1` (single bead), capped at 50 for batch preview.
- `--type` and `--priority` filter via the same query params as
  `bd list` (parse_priority_flag for normalization).
- Without `--next`: clear error pointing at the follow-up's
  interactive scope.
- Wired into `BD_SUBCOMMANDS`, dispatcher route, and MCP catalog
  (`agent_safe: true, idempotent: true, mcp_enabled: true`).

## Diff summary

- Commit: `9cd35e95`.
- Files: `crates/caco-cli/src/lib.rs` (+111: `BD_TRIAGE_ARGS`,
  `CommandSpec` entry, dispatcher route, `dispatch_bd_triage` fn).
- Tests: smoke-validated locally:
  - `caco bd triage --next --limit 2` prints 2 oldest drafts.
  - `caco bd triage --next --type bug --priority P2 --limit 3`
    returns the filtered subset.
  - `caco bd triage` (no flags) errors with helpful pointer.
- `cargo build -p caco-cli` and `cargo clippy -p caco-cli`: clean.

## Out of scope (deferred to bd-eef036)

- Interactive `[P]romote / [D]iscard / [M]erge-into / [F]efer /
  [L]abel / [S]kip / [Q]uit` per-bead loop.
- Auto-dup detection by title-similarity.
- Per-session cap (`--max N` to prevent operator burnout).

## Operator-takeaway

`caco bd triage --next --limit 5 --json` is now your "what should I
look at first" pipe. Combine with `caco bd update --bead-id <id>
--status open` or `caco bd close --bead-id <id>` to manually
process drafts. The interactive loop that automates the prompt UX
will land in bd-eef036.
