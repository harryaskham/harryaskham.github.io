# Session summary — caco-summary backend (daemon API + CLI)

## Goal

Build the shared backend that the three planned session-summary viewer
UIs (TUI / caco-web / Android) consume so they don't each reimplement
markdown parsing, bead-ID extraction, and artefact scanning. Operator
(Harry) reported that summaries authored by the session-recording mixin
were effectively write-only — durable artefacts on disk with no surface
to browse them.

## Bead(s)

- `bd-41b916` — caco summary CLI + daemon API: enumerate session summaries
- parent epic `bd-a5e2fa` — Session-summary viewers across TUI, caco-web, and Android
- siblings (still open): `bd-ba7239` (TUI), `bd-a0503e` (web), `bd-806014` (Android)

## Before state

- `.cacophony/agent/<id>/summary/<idx>/summary.md` artefacts existed on
  disk (3 examples already committed) with no daemon endpoint or CLI to
  enumerate or read them.
- The session-recording mixin's seven-section schema (Goal / Bead(s) /
  Before / After / Diff / Embedded artefacts / Operator-takeaway) was
  documented in profile prose only — no parser shared the contract.
- Reintegration validation (`reintegration::validate_recorded_summary`)
  was the only consumer of the schema, and it only checked existence
  and section presence, not parsed content.
- `cargo test -p caco-daemon --lib summary::`: no such tests.

## After state

- New module `crates/caco-daemon/src/summary.rs` (≈540 lines incl.
  tests) with:
  * `SummaryRecord` (list-view projection: agent_id, project, index,
    timestamp, title, bead_ids, artefacts, summary_path, bytes)
  * `ArtefactSummary` (has_cast / has_data_json / screenshots[])
  * `ParsedSummary` (record + full body + `SummarySections` strongly-
    typed view of all seven canonical sections plus `extra_sections`
    catch-all)
  * `enumerate_summaries(checkout, filter)` — walks every agent dir,
    tolerates missing/malformed indices, sorts most-recent-first with
    a deterministic tiebreak.
  * `read_parsed_summary(checkout, project, agent, index)` — returns
    a fully-parsed structured summary or None.
  * Hand-rolled `extract_bead_ids` (regex-free) that respects identifier
    boundaries — rejects `xxbd-123456`, `bd-1234567`, `bd-ABCDEF`, etc.
- New routes in `caco-daemon/src/lib.rs`:
  * `GET /api/v1/summaries` (project / agent_id / bead_id filters,
    limit + offset paging, returns `{items, total, limit, offset}`)
  * `GET /api/v1/summaries/{agent_id}/{index}` (project disambiguation
    via query string, walks every configured project on omission)
  * Both registered in `check_agent_scope` as read-only so workers
    can call them.
- New CLI surface in `crates/caco-cli/src/summary_cmd.rs` + lib.rs:
  * `caco summaries list` (paginated, filterable, text + --json)
  * `caco summaries show --agent <id> --index <n>` (full parsed render
    including seven canonical sections, bead refs, artefact summary)
  * `summaries` is a CommandSpec branch with full ArgSpecs so bd-b76723
    unknown-flag warnings stay silent.
- Tests: `cargo test -p caco-daemon --lib summary::` — 9/9 passing.
- Lints: `cargo clippy -p caco-daemon -p caco-cli --no-deps` clean.

## Diff summary

- Commit: `3de87e6b8`
- Files touched (all additive):
  * `crates/caco-daemon/src/summary.rs` (new, +540 lines)
  * `crates/caco-daemon/src/lib.rs` (+~180 lines: routes, handlers,
    scope allow, mod decl)
  * `crates/caco-cli/src/summary_cmd.rs` (new, +290 lines)
  * `crates/caco-cli/src/lib.rs` (+~85 lines: mod decl, ARG specs,
    SUMMARIES_SUBCOMMANDS, dispatch arms, top-level branch)
- Tests: +9 / -0 / flipped 0
- Behavioural delta: a new read-only API namespace and CLI subcommand
  exposing structured session summaries; no existing surfaces changed.

## Operator-takeaway

The session-recording mixin's seven-section schema is now a first-class
parsed type (`SummarySections`) rather than profile prose. Future UIs
should consume `GET /api/v1/summaries{,/<id>/<idx>}` and never reparse
markdown themselves — both endpoints share the same `SummaryRecord`
projection so list and detail views can render with one data path.
The bead-ID extractor is hand-rolled (no regex dep churn) and
identifier-boundary aware; if the canonical bead-ID format ever widens
beyond `bd-[0-9a-f]{6}` we update one function instead of grepping
every UI.
