# bd-e4cd9d — daemon: all-projects links list endpoint GET /api/v1/links (bd-c3ed14 slice 1)

## Bead
bd-e4cd9d (daemon/api/files-page/links, P2) — slice 1 of parent bd-c3ed14 "Decouple files list backend from caco-web frontend". caco-dev-po4-1 investigated bd-c3ed14, decomposed it, and explicitly flagged this slice as "the clean in-lane enabler if a daemon/Rust worker wants to start there." Coordinated with po4-1 (notified) and noted the child on the parent.

## Problem
The daemon already serves all-projects FILES via `GET /api/v1/file-cache` (bd-6eaabe) + content via `GET /api/v1/file-cache/<id>/content`, but had NO all-projects LINKS list — only project-scoped `GET /api/v1/projects/<project>/links` (`handle_project_links_list`). caco-web's files view (`serve_web_files`) merges files + links across ALL projects, so the caco-web frontend could not move off the caco-web `/api/v1/web/files` provider without dropping links. This slice adds the symmetric all-projects LINKS read so the daemon is the full files+links provider.

## Change (crates/caco-daemon/src/lib.rs)
- `AllProjectLinksQuery { project, query, limit }` — mirrors `ListFileCacheQuery`.
- `project_link_matches(record, query)` — case-insensitive substring over id/project/url/host/title/creator/tags/bead_id (mirrors the file-cache `record_matches` set).
- `read_all_project_link_records(runtime_root)` — enumerates `<root>/files/<project>/links.jsonl` across every project dir, per-project dedup by id (last write wins, `BTreeMap`), drops inactive records, skips dirs with no `links.jsonl`, returns empty on a missing files root (matches `read_project_link_records` semantics).
- `filter_all_project_link_records(records, query)` — optional project scope, optional substring query, newest-first by `created_at`, bounded limit (clamp 1..=1000).
- `handle_all_project_links_list` — `GET /api/v1/links`; reads via `spawn_blocking`, returns `{ scope, project, count, links }` (mirroring the file-cache list envelope); `links_read_error` 500 on read failure.
- Route registered next to `/api/v1/file-cache` on the **local bearer router only**, mirroring the bd-6eaabe file-cache read decision; cluster mTLS client-node exposure stays the bd-754c27 operator-gated authz decision.
- Auth scope allowlist: added `path == "/api/v1/links"` next to the file-cache entry (read-only operational endpoint, agent-scope reachable like file-cache).

## Out of scope (remain on parent bd-c3ed14 for owning surfaces)
Slice 2 caco-web `loadFilesView` migration to `/api/v1/file-cache` + `/api/v1/links` (needs browser validation); slice 3 removal of the `serve_web_files` / `web_file_records` provider; slice 4 the other-surface consumers (macOS/iOS/watchOS/Android/wearOS/TUI).

## Tests
`all_project_links_list_merges_filters_and_sorts_bd_c3ed14` (mirrors `project_links_http_helpers_match_cli_json_shape_bd_4e82e8`): writes two project link stores (alpha with an active + an inactive record, beta with one active) plus an empty `gamma` dir; asserts merge across projects, inactive dropped, newest-first sort, project scope, case-insensitive substring query, limit, and that a missing files root yields an empty list (no error).

## Validation (daemon test queue, --cwd at checkout)
- `cargo test -p caco-daemon --lib bd_c3ed14` (tj-489a6e5c): PASSED 1/1 (lib compiled).
- `cargo clippy -p caco-daemon --lib` (tj-dcf39c25): PASSED, warning-clean (no own-file warnings).
- `git diff --check` clean; inserted lines rustfmt-clean (verified via skip_children reformat diff — pre-existing whole-file drift untouched). Incidental Cargo.lock version-bump/new-`pico`-crate churn discarded (not part of this change).

## Diff
See the reintegration receipt for the landed squash SHA.
