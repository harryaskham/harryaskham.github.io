# bd-9c9bca — daemon-side producer guard: stop storing internal cluster-API URLs in user-facing record url fields

## Bead
bd-9c9bca (api-url/daemon/follow-up, P2 bug; filer caco-ctrl). Follow-up to bd-d19c24 (macOS external-open leak) and my bd-80801c (the serialization-boundary sanitizer). The internal-API URL originates daemon-side, stored in a record's `url` field then surfaced to user-facing panes. Asks for a producer-side fix so internal cluster-API URLs are never stored in user-facing `url` fields in the first place.

## Audit finding
The daemon does NOT format internal-API URLs into record url fields (grepped caco-daemon/caco-cli — none). The file-cache upload handler (`POST /api/v1/file-cache`) shells out to the canonical `caco file add` writer and passes `--project/--content-file/--name/--mime/--title/--description/--tags/--associations/--json` but NEVER `--url`. So an internal-API URL only enters a record's `url` field when a client explicitly passes `caco file add --url <internal>` / `caco link add --url <internal>` — which is always a bug (a user-facing openable url should never be an internal `/api/v1` peer-API endpoint). Rejecting at the writer is therefore safe (no daemon-upload breakage).

## Fix (crates/caco-cli/src/file_cmd.rs)
- `reject_internal_api_url(&str)`: errors if the url is an internal cluster-API endpoint (reuses my bd-80801c `caco_config::is_internal_api_url`, port-agnostic, preserves the caco-web `/api/v1/web/` dashboard namespace).
- `checked_user_facing_url(Option<String>)`: validates an optional url, rejecting internal endpoints, returning it unchanged when acceptable.
- Applied at all 4 client url-set sites: file add (`url:`), file update (`record.url =`) via `checked_user_facing_url`; link add + link update (url required) via `reject_internal_api_url`.
- Defense-in-depth complementing the bd-80801c list/display sanitizer + the macOS ExternalOpenGuard.

## Validation
- `cargo clippy -p caco-cli --lib` (tj-4d302e99): exit 0, clean.
- Focused tests `reject_internal_api_url_blocks_cluster_endpoints_bd_9c9bca` + `checked_user_facing_url_rejects_internal_passes_others_bd_9c9bca` added (pure-fn assertions: internal→err, external→ok, /api/v1/web/→ok, None→ok). The daemon test-queue endpoint went non-responsive (daemon_endpoint_nonresponse, infra wedge) so the focused run could not complete locally; the reintegration merge-queue gate (cargo test-small + cargo check --workspace + cargo clippy --workspace on the merge commit) covers these lib tests.
- rustfmt-clean; git diff --check clean. AGENTS.md caco-file note extended with the producer-side guard.

## Diff
See reintegration receipt for the landed squash SHA.
