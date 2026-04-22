# Session summary — bd-d4e93d caco msg history

## Goal

Land bd-d4e93d, the historical-search counterpart to
`caco msg inbox` — `caco msg history --since/--until/
--grep/--type/--from/--to/--tail` so postmortem and audit
flows can grep the chat log without polling the unread
inbox or pulling the entire DB.

## Bead(s)

- `bd-d4e93d` — caco msg history search.

## Diff summary

**`crates/caco-cli/src/lib.rs`:**

- New `MSG_HISTORY_ARGS` (`--project` required;
  `--since`, `--until`, `--tail` (default 100), `--grep`,
  `--type`, `--from`, `--to` optional).
- New `msg history` entry in `MSG_SUBCOMMANDS`
  (mcp_enabled, agent_safe, idempotent — read-only).
- New `dispatch_msg_history` (~140 lines): calls existing
  `/api/v1/projects/{p}/messages/chat?limit=N` (which
  returns ALL messages, read or unread), applies
  client-side filters in this order: `--type` (exact kind
  match), `--from` (sender substring), `--to` (target
  substring), `--grep` (case-insensitive body substring),
  `--since` / `--until` (timestamp range). Bounds the
  filtered result to `--tail N` post-filter so client-side
  filters don't shrink results below the cap inadvertently.
- New `parse_history_when` helper: accepts RFC3339
  timestamps OR duration suffixes (`30s`, `5m`, `2h`,
  `1d`). Durations resolve relative to `Utc::now()`.
- Stable JSON envelope:
  `{ok, data: {project, requested_tail, returned, messages}}`.
- Text mode: one line per message with body truncated at
  200 chars.
- 2 new tests: spec exposure (`msg_history_subcommand_exposed_in_spec`)
  and parser rules (`msg_history_parse_when_accepts_rfc3339_and_durations`).

## Before state

- Postmortem flows had to pull the full chat log via
  `/api/v1/projects/{p}/messages/chat?limit=10000` and
  grep client-side every time.
- `caco msg inbox` only searches *unread* messages —
  searching read history required raw DB access.
- This bead was filed by caco-ctrl tonight after spending
  effort grepping for cluster-ctrl threshold-decision
  context in the inbox tail.

## After state

- `caco msg history --project P --since 1h --grep
  cluster-ctrl --tail 50` finds the recent threshold
  conversations in one shot.
- `--since` / `--until` accept either RFC3339 or
  duration suffix — natural UX for both "last hour" and
  "specific incident window" queries.
- JSON envelope is stable; pairs with bd-e33b44 `msg
  snapshot` (now both call into the same chat / inbox
  endpoints with structured envelopes).
- `cargo test-small` 56/56 green (bd-10e37c flake from
  earlier this session has cleared on main — msm-3 must
  have landed the fix).
- `cargo test -p caco-cli --lib msg_history` 2/2 green.
- `cargo build -p caco-cli` green.

## Notes / verification

- Reuses `apply_provenance` / `async_send_request` and
  the existing chat endpoint — no daemon-side change
  required for slice 1.
- Filter ordering puts cheap exact-match filters before
  expensive substring/regex passes for large result sets.
- `--tail` is enforced post-filter so a query like
  `--tail 50 --grep foo` returns the 50 most recent
  matches, not the 50 most recent messages of which only
  some match.

## Out of scope

- `--thread-of <msg-id>` filter — depends on bd-f8f754
  threading (msm-2 owns).
- Daemon-side filter pushdown (`?since=`, `?grep=`) —
  current `/messages/chat` endpoint only takes `limit=`.
  Would be a useful slice 2 once this CLI gets used
  enough to hit the limit-fetch ceiling. File a follow-up
  if/when needed.
- `--cron` type filter — current message kinds are
  direct/broadcast/speak/system; no cron kind exists yet.

## Operator-takeaway

`caco msg history --project P --since 1h --grep
cluster-ctrl --tail 50` is now the one-shot way to grep
the chat log for postmortem/audit. Pairs with bd-e33b44
`msg snapshot` (both now expose stable JSON envelopes
over the existing inbox/chat endpoints).
