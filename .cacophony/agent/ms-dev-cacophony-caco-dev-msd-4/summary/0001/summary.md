# Session summary — bd-a9ba6b choices show not-found semantic alignment

## Goal

Make `caco choices show --choice-id bogus` exit non-zero with a
`{ok:false, error:{code:not_found,...}}` JSON envelope, matching
`caco bd show` and `caco scratch show`, so scripts gating on `.ok`
or `$?` see consistent semantics across sibling `*show` surfaces.

## Bead(s)

- `bd-a9ba6b` — caco choices show --choice-id bogus --json returns {ok:true,data.found:false,exit 0} treating not-found as SUCCESS — opposite of bd show + scratch show

## Before state

- `caco choices show --choice-id bogus --json` returned `{ok:true, data:{found:false}, meta:{...}}` with exit 0.
- `caco bd show --bead-id bogus --json` returns `{ok:false, error:{code:not_found,...}}` with exit 1; `caco scratch show --note-id bogus --json` mirrors that. Three sibling *show surfaces, two semantics — a hidden trap for any script doing `caco choices show ... | jq -e '.ok'`.
- Empty-string bypass: `--choice-id ""` was passed through `flags.get("--choice-id")` (only `None` triggered the `ok_or_else` error), then sent to the daemon as a not-found lookup that came back wrapped in the success-on-not-found mapping.
- Failing tests: bd-c19193 (pre-existing, unrelated).

## After state

- `dispatch_choices_show` rendering split into a pure helper `render_choices_show_response(resp, choice_id, json_requested) -> Result<String, CliError>` so the not-found semantics are unit-testable without a live daemon.
- For `data.found == false`:
  - text mode → `Err(CliError::new("choice not found: <id>"))` (exit 1).
  - JSON mode → `Err(CliError::new("<reshaped envelope>"))` carrying `{ok:false, error:{code:not_found, message:...}, meta:{...}}`; upstream `meta` (including `request_id`) is preserved so triage isn't lost.
- For `data.found == true`: renderer is unchanged — JSON pass-through, text summary with the bd-b655b6 recommended-star marker.
- Empty-string guard at the dispatch site: `--choice-id ""` errors with `--choice-id must not be empty` before any HTTP call.
- Daemon-side `/api/v1/choices/<id>` semantics are intentionally unchanged to avoid cross-cutting other consumers; the CLI does the local re-shape.
- Failing tests: bd-c19193 (unchanged, pre-existing).

## Diff summary

- Commit: `b395ba6b bd-a9ba6b: align caco choices show not-found semantics with sister *show commands`
- Files touched: `crates/caco-cli/src/lib.rs` (+174 / -39).
- Tests: +4 / -0 / flipped 0
  - `bd_a9ba6b_choices_show_not_found_text_mode_returns_err`
  - `bd_a9ba6b_choices_show_not_found_json_mode_returns_err_with_reshape` (asserts envelope structure + meta preservation)
  - `bd_a9ba6b_choices_show_found_text_mode_renders_summary` (regression guard for the happy path including recommended-star)
  - `bd_a9ba6b_choices_show_found_json_mode_passes_through`
- Behavioural delta: not-found now exits 1 with the `{ok:false,error:{code:not_found,...}}` envelope across both text and JSON modes; found responses are unchanged. Empty `--choice-id` is refused at the CLI before any daemon call.

## Operator-takeaway

The semantic divergence is closed at the CLI layer. Scripts can now
rely on `caco bd show / caco scratch show / caco choices show` all
following the same not-found contract. The bead also flagged
adjacent issues (84 unavailable choices, cron list envelope shape,
inline allowed-values pattern) — those are out of scope here and
will need their own beads; this fix narrowly closes the
`choices show` divergence and the `--choice-id ""` bypass.
