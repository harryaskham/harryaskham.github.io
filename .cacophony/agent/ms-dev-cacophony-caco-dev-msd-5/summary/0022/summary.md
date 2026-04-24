# Session summary — bd-77fc56 chat list body projection

## Goal

Apply the bd-eb1b56 metadata-only-by-default projection pattern to
`/api/v1/projects/<p>/messages/chat` + `/api/v1/messages/chat` —
the RED audit finding #2 from bd-c97e14 (sibling of bd-491508 which
landed the same pattern on beads list).

## Bead(s)

- `bd-77fc56` — Apply bd-eb1b56 projection pattern to
  `/api/v1/messages/chat` (omit body by default; surface body_len +
  `?include_body=true`)

## Before state

- `MessageResponse { body: String, ... }` always shipped the full
  body for every message on every chat list response.
- ctrl broadcasts + agent narratives are multi-KB each × tail=100 =
  inflated default payload.
- bd-c97e14 audit catalogued this as RED #2 (largest runner-up after
  beads description).

## After state

- New `BodyProjection` enum (`Omit` / `Full` / `Preview(N)`) with
  `from_params` resolving include_body > body_preview > Omit
  precedence (mirrors bd-491508's DescriptionProjection on beads list
  and bd-eb1b56's content projection on scratch list).
- `MessageResponse` gains `body_len: usize` (always emitted) and
  `body_truncated: bool` (skipped when false).
- New `MessageResponse::project_from(msg, projection)` ctor; the
  legacy `From<&Message>` impl delegates to it with `Full` so
  non-list call sites (inbox, thread, snapshot, MCP) stay back-compat
  AND get `body_len` populated for free.
- `ChatParams` gains `include_body: bool` and
  `body_preview: Option<usize>`.
- Both `handle_project_chat` + `handle_global_chat` thread the
  projection through `.map(...)`. Default is Omit.
- CLI `MSG_HISTORY_ARGS` adds `--metadata-only` and `--body-preview`.
  `dispatch_msg_history` threads them; default is `?include_body=true`
  for text-mode + --grep back-compat.
- `dispatch_msg_thread` + `dispatch_msg_snapshot` append
  `include_body=true` (they render bodies).

## Diff summary

- 2 files modified (caco-daemon/src/lib.rs, caco-cli/src/lib.rs),
  232 insertions / 6 deletions.
- 7 new unit tests covering all projection modes + precedence +
  back-compat From impl.
- `cargo check --workspace --tests`: green (field-add discipline
  satisfied: MessageResponse construction goes through From or
  project_from; no external construction sites).
- `cargo test-small`: green.

## Broken-on-main assessment

`bd0502bc_node_disk_rejects_non_local_node` stack-overflow reported
by wmi-2. wmi-2's follow-up confirms test is PRE-EXISTING on HEAD^
and passes with `RUST_MIN_STACK=33554432`. Not a live regression —
environment-dependent; unrelated to this change (touches daemon
lib.rs + caco-cli msg history only, not disk_breakdown.rs).

## Out of scope

- `/api/v1/outbox` payload projection — filed as bd-63203d (RED #3).
- `/api/v1/msg/inbox` body projection — same pattern, lower priority
  (inbox is per-agent, smaller hit).

## Operator-takeaway

Second-largest list-endpoint payload regression closed. Every text-
mode and --grep CLI flow stays unchanged. Operators opt into the
lean projection via `caco msg history --metadata-only` or
`--body-preview N`. HTTP consumers that need the body add
`?include_body=true`.
