# Session 0010 — bd-6ff0a0 profile auto-include investigation

## Outcome
Closed bd-6ff0a0 as already-resolved. On-disk profile discovery
(bd-170e88, bd-0e9d94) already serves profiles from disk at runtime
without requiring a `cargo build` regeneration. Filed bd-cc985b and
bd-909491 as follow-up drafts for `caco profile list` CLI and
`--profile-path` respectively.

## Analysis
Investigated the full profile resolution chain:
- `profile_search_dirs` scans 4 directories in precedence order
- `resolve_profile_with_overrides` checks on-disk first, embedded
  fallback second
- `list_profile_infos` merges both sources (on-disk wins)
- `/api/v1/profiles` endpoint surfaces all discovered profiles
- After reintegration, newly authored profiles are immediately
  discoverable on disk without rebuilding the binary

## Friction beads
- bd-cc985b (draft, p3): `caco profile list`/`show` CLI wrappers
- bd-909491 (draft, p4): `--profile-path` direct file loading
