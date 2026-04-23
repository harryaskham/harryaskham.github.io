# Session summary — bd-cc985b caco profile list / show CLI

## Goal

Add thin CLI wrappers for /api/v1/profiles so operators don't have
to spawn an agent or curl the API to inspect discovered profiles.

## Bead(s)

- `bd-cc985b` — Add caco profile list / show

## Before state

- /api/v1/profiles existed (bd-012d92) but no CLI surface
- Endpoint denied worker scope (operators inside agents couldn't
  reach it)

## After state

- `caco profile list [--source repo|legacy|daemon|user|embedded]`
- `caco profile show --name NAME`
- Both registered as mcp_branch with full ArgSpec + dispatch arms
- /api/v1/profiles added to is_read_only_info_endpoint so worker-scope
  agents can read it (matches /api/v1/projects, /api/v1/nodes, etc.)
- Source filter validated against documented enum
- Show prints key:value metadata or raw JSON

## Diff summary

- Commits: 6c53df94e06f
- Files: `crates/caco-cli/src/lib.rs` (+243), `crates/caco-daemon/src/lib.rs` (+5)
- Tests: +2

## Operator-takeaway

This session: 6 beads landed (bd-7189e7, bd-df639c, bd-2f7850,
bd-d0eb7b, bd-dcc3ac, bd-cc985b) plus 9 test-health cycles on
bd-274c2d. Standing down per operator directive after this
reintegrate. Permanent claims bd-274c2d + bd-1c0bdd preserved
(per workflow: never close).
