# Session summary — bd-4e82e8 project-scoped links HTTP API

## Goal

Address `bd-4e82e8`: expose project-scoped canonical link records over daemon HTTP so clients such as Android can list/search/show caco link records without shelling out to the CLI.

## Changes

- Added local and cluster routes:
  - `GET /api/v1/projects/{project}/links`
  - `GET /api/v1/projects/{project}/links/{link_id}`
- Added daemon-side `ProjectLinkRecord` matching the canonical CLI link JSON fields.
- Added JSONL reader for the per-project `files/<project>/links.jsonl` store.
- Added filters for `tag`, `host`, `bead_id`, `creator`, and bounded `limit`.
- List response returns `{ project, count, links }`; show response returns `{ link }`, matching CLI envelope data shape.
- Added focused regression covering canonical fields, active-record filtering, sort/limit behavior, and filter parity.

## Validation

- `cargo test -p caco-daemon --lib project_links_http_helpers_match_cli_json_shape_bd_4e82e8 -- --test-threads=1`
- `git diff --check`

## Diff summary

- Code/content commit: `bb4cc160a1`
- Summary artefact commit: omitted intentionally; reintegration receipt is the source for the final landed squash SHA.
