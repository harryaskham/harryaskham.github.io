# bd-80801c — daemon must not expose internal cluster /api/v1 URLs in user-facing record url fields

## Bead
bd-80801c (artifact/cluster-port/daemon/file-cache/link/security/url, P2; filer caco-ctrl). Daemon-side ROOT CAUSE of bd-d19c24 (macOS app opened a cluster `:12100/api/v1` URL in Edge; caco-macos landed a client `ExternalOpenGuard` at 51c857234). User-facing record `url` fields (file-cache/link/artifact) must never carry internal daemon-local / cluster-mTLS `/api/v1` peer-API URLs (e.g. `https://<cluster_addr>:12100/api/v1/file-cache/<id>/content`), or Android/web/any consumer inherits the same leak.

## Investigation
`caco file/link add` write records to on-disk `index.jsonl` from caller-provided `--url` (file_cmd.rs); the daemon serves them via `GET /api/v1/file-cache` (list) — the macOS app hit that list and opened the URL it received. So the leak is *any producer* passing an internal URL; the robust fix is a guard that catches any internal-API URL at the serialization boundary regardless of producer (mirroring the macOS guard), so consumers never receive one. Persistence uses the struct's serde serialization (not the display serializers), so sanitizing the display path does not corrupt stored data.

## Fix
- **caco-config** `is_internal_api_url(url) -> bool` (shared, port-agnostic): http(s) URL whose path is under `/api/v1/` (or exactly `/api/v1`), EXCEPT the browser-facing caco-web dashboard `/api/v1/web/` namespace (the bead's explicitly-acceptable alternative). Mirrors the macOS `ExternalOpenGuard`. + 2 unit tests.
- **caco-daemon/file_cache.rs** `sanitize_internal_api_url_field`: in `handle_list_file_cache` (the only daemon file-cache url-serializing route; `/content` serves bytes), blank `url`/`content_url` fields that are internal-API URLs before the records reach any consumer (macOS/Android/web). + 1 unit test.
- **caco-cli/file_cmd.rs** `user_facing_url_value`: applied in the shared `record_to_json` + `link_record_to_json` display serializers, so ALL CLI file/link display paths (list/show/get/search/update/delete output) are sanitized for parity. Display-only — persisted index.jsonl is unchanged. + 1 unit test.
- **AGENTS.md** caco-file family note updated.

## Validation (daemon test queue, --cwd at checkout)
- `cargo test -p caco-config internal_api_url_tests` (tj-8d6a6b1d): 2 passed.
- `cargo test -p caco-daemon --lib sanitize_blanks_internal` (tj-b876c037): 1 passed.
- `cargo test -p caco-cli --lib user_facing_url_value` (tj-58b776c5): 1 passed.
- `cargo clippy -p caco-config -p caco-daemon -p caco-cli --lib` (tj-dfd0abe0): exit 0 (only pre-existing non-mine "very complex type" warnings; not `-D`).
- rustfmt-clean on changed regions; `git diff --check` clean.

## Scope / follow-ups (noted for caco-ctrl)
Covered the primary bd-d19c24 surfaces (daemon file-cache list + all CLI file/link display). Deferred as separate slices: (a) a creation-time guard (reject/strip internal-API `--url` at `caco file/link add` so they are never stored — left out here to avoid producer-breakage risk; sanitizing at serialization already protects all consumers); (b) the UI-snapshot artifact url fields mentioned in the bead pointers (a distinct surface from the file-cache/link substrate).

## Diff
See the reintegration receipt for the landed squash SHA.
