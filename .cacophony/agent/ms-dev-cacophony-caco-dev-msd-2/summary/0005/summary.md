# Session summary — agents read previous summaries on startup (bd-20d2dc)

## Goal

Operator quote: "agents on startup should look at their previous
summaries". Make every spawned agent see its most recent reintegration
summaries injected into its context-on-disk surface (CLAUDE.md /
AGENTS.md) so it has continuity across reintegrations and recreate
cycles.

## Bead(s)

- `bd-20d2dc` — Agents should read their previous summaries on startup.

## Before state

- `inject_managed_claude_md` / `inject_managed_codex_agents_md`
  injected only static no-Plan-mode instructions on every spawn.
- Agents started with no awareness of their own session history.
  Recreate cycles (operator-directed re-spawn after stranding) lost
  all prior reflection completely; reintegrate-then-resume cycles
  required the agent to manually `caco summary list` for itself.
- The `cacophony-state` branch already commits summaries via
  `bd-fddde3` (reintegrate split), and `summary::enumerate_summaries`
  already walks the working-tree path. No surface read them at
  startup.

## After state

- New `inject_recent_summaries_block(checkout, agent_id, count=5)` in
  `crates/caco-daemon/src/agent/spawn.rs`.
- Wired into `lifecycle.rs` directly after the existing
  `inject_managed_claude_md` / `inject_managed_codex_agents_md` calls,
  so it runs unconditionally on every spawn (cold + recreate +
  pool-resume that re-runs the spawn handshake).
- The injected block is sentinel-bracketed:
  `<!-- BEGIN AUTOGEN: previous-summaries (bd-20d2dc) -->` ...
  `<!-- END AUTOGEN: previous-summaries (bd-20d2dc) -->` and
  REPLACED on every spawn (not appended) so it always reflects the
  latest 5 entries, in most-recent-first order.
- Block content: short markdown table with `# | Title | Timestamp |
  Path` rows. Title taken from each summary's first H1; agents
  follow up to the listed `summary_path` for full reads.
- Empty-list case: still emits sentinel pair with a "no previous
  summaries on disk" notice so the block is deterministic and the
  next spawn can replace it cleanly.
- Opt-out via `CACO_DISABLE_PREVIOUS_SUMMARIES_INJECTION=1` in the
  spawn env. When set, neither CLAUDE.md nor AGENTS.md is touched.
- Non-fatal: errors are logged via stderr (no `tracing` dep added)
  and the spawn continues. Better to spawn an agent without history
  than to block spawn on a docs-injection edge case.
- 8 new tests in `crates/caco-daemon/src/agent/tests.rs`:
  - `inject_recent_summaries_block_creates_sentinels_in_new_file`
  - `inject_recent_summaries_block_is_idempotent` (key invariant)
  - `inject_recent_summaries_block_replaces_existing_block`
    (validates user content above/below survives, sentinels stay
    uniquely paired)
  - `inject_recent_summaries_block_handles_no_summaries_gracefully`
  - `inject_recent_summaries_block_respects_opt_out_env`
  - `render_recent_summaries_block_orders_most_recent_first`
  - `render_recent_summaries_block_truncates_to_count`
  - `render_recent_summaries_block_escapes_pipe_in_title`
  - All passing; clean `cargo build -p caco-daemon`.

## Diff summary

- `crates/caco-daemon/src/agent/spawn.rs`: +`inject_recent_summaries_block`,
  `render_recent_summaries_block`, `upsert_summaries_block`,
  `escape_markdown_table_cell`, `RECENT_SUMMARIES_BEGIN/END` consts
  (+~140 LOC).
- `crates/caco-daemon/src/agent/lifecycle.rs`: 1 call site after
  existing CLAUDE.md/AGENTS.md inject (+5 LOC, comment-heavy).
- `crates/caco-daemon/src/agent/tests.rs`: +8 tests + `write_fake_summary`
  helper (+~180 LOC).

## Operator-takeaway

On the next spawn (or recreate) of any agent, its CLAUDE.md and
AGENTS.md will gain a "Previous summaries" section listing the most
recent 5 reintegration summaries it authored, sourced from the
working-tree `.cacophony/agent/<id>/summary/` path (which the
`cacophony-state` reintegrate flow keeps refreshed). Agents reading
the file at startup get continuity for free.

State-branch sourcing (`enumerate_summaries_from_state_branch`) is
NOT yet wired in — slice 1 reads working-tree only. Working-tree is
correct for fresh + post-reintegrate spawns because reintegrate syncs
the checkout to main before completing. State-branch sourcing for
the "fresh node hydrate from remote" case is a follow-up slice; the
helper is already factored to make that swap a one-line change.
