# Summary 0010 — bd-9cd238: caco agent log daemon endpoint + remote forwarding

## Bead
bd-9cd238 (P3, feature) — `caco agent log` daemon endpoint + remote
forwarding. Self-filed follow-up to bd-83a84d (which landed local-only).

## Change

`crates/caco-daemon/src/lib.rs`:
- New route `GET /api/v1/agents/{agent_id}/log` registered on both
  the local and cluster routers (matches sibling `/logs` and `/diff`
  registration pattern).
- New `AgentLogQueryParams { mode: Option<String>, count: Option<usize> }`
  for axum `Query<>` extraction.
- New `handle_agent_log_endpoint` async handler:
  - Validates `mode` (default `tail`; rejects unknown with 400
    `invalid_mode`).
  - Validates `count` (default 100; rejects 0 with 400 `invalid_count`).
  - Locates local agent dir by scanning `paths.agents/<project>/<id>`.
  - Resolves tmux session+socket from in-memory state, falling back
    to on-disk `agent.json` (rare daemon-restart rehydration window).
  - Runs capture in `tokio::task::spawn_blocking` so the async
    runtime stays unblocked.
  - For `head`, captures full buffer then slices first K locally
    (tmux capture-pane has no native head primitive).
  - For `tail`, uses bounded `capture_tmux_pane_content_lines(K)`.
  - For `all`, uses `capture_tmux_pane_content_full`.
  - On local miss, mirrors the `/diff` peer-forwarding pattern:
    `resolve_remote_agent_node` → `forward_agent_get_to_peer` with
    the original mode+count preserved in the suffix path.
  - Returns SuccessEnvelope wrapping
    `{ ok, id, mode, requested_lines, captured_lines, content }`.

`crates/caco-cli/src/lib.rs`:
- `dispatch_agent_log` extended with the same fall-through pattern
  used by `dispatch_agent_logs` (bd-9f9da8): when the agent
  runtime dir is not local, query the daemon endpoint at
  `/api/v1/agents/{id}/log?mode=...&count=...`.
- `--json`: returns the daemon body verbatim.
- Plain mode: extracts `data.content` from the SuccessEnvelope and
  prints it (or a "tmux pane is empty or session is dead (remote)"
  diagnostic if content is empty).
- Falls through to a clear error if the daemon is unreachable.

## Tests

`crates/caco-daemon/src/lib.rs::tests::`:
- `agent_log_endpoint_rejects_invalid_mode` — 400 + `invalid_mode`
  for `?mode=bogus`.
- `agent_log_endpoint_rejects_zero_count` — 400 + `invalid_count`
  for `?count=0`.
- `agent_log_endpoint_returns_404_for_unknown_agent` — 404 +
  `agent_not_found` when no peer owns the agent.
- `agent_log_endpoint_defaults_to_tail_100` — bare `/log` request
  is accepted (does NOT 400 on missing query params).

End-to-end happy path requires a real tmux server, which the test
harness does not provide; the helper itself is exercised by
`capture_tmux_pane_content_full_returns_none_for_missing_session`
landed under bd-83a84d.

## Verification

- `cargo check -p caco-daemon` — clean.
- `cargo check -p caco-cli` — clean.
- `cargo test -p caco-daemon --lib agent_log_endpoint` — 4/4 green.
- `cargo test-small` — 4201/4201 green
  (197+109+720+291+18+2814+52 across small-suite crates).

## Operational impact

- `caco agent log --id <remote-agent>` now works transparently
  cluster-wide, mirroring `caco agent logs` and `caco agent diff`.
- New endpoint is read-only; auth scope identical to peer routes
  on the same router.
- No behaviour change to existing endpoints.
- No new daemon dependencies; uses the existing
  `forward_agent_get_to_peer` cluster mTLS path.

## Deferred (still open)

- bd-83a8ed: `caco agent log --since TS` for timestamp-bounded
  scrollback. Filed under bd-83a84d follow-up; orthogonal design
  needed (tmux capture-pane has no native per-line timestamps).

## Next

Reintegrate direct, close bd-9cd238, idle.
