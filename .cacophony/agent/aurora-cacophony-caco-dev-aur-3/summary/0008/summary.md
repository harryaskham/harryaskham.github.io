# Session summary — Group-scoped web chat fanout

## Goal

Implement the first group-scoped chat slice for agent groups so an operator can start from an agent group and send one message directly to every agent in that group, without inventing a parallel messaging protocol or touching mobile/specialist chat lanes.

## Bead(s)

- `bd-6d30f2` — Implement group-scoped chat functionality

## Before state

- The web Nodes detail surface already grouped agents by project and had per-agent DM controls, but no group-level chat affordance.
- The main dashboard chat composer supported broadcast, speak, and direct agent sends, but not a concrete group target.
- The standalone `workspace-chat-pane.js` supported send/speak/broadcast and agent-selected bus targeting, but not agent-group-selected/open-group-chat scoping.
- Beads-primary was flapping for much of the session, so ownership/coordination used project messages plus git history per Harry's directive.

## After state

- Nodes `Agents by project` groups now expose a `Chat` action that scopes chat to that group's concrete agent IDs.
- The main dashboard chat target picker can hold an encoded group target and fans messages out via one canonical `/api/v1/projects/<project>/messages/send` direct send per group member.
- The standalone workspace chat pane also supports `group` mode, `agent-group-selected` / `open-group-chat` bus events, a visible group scope header, and per-agent canonical direct-message fanout.
- Local commit `65d8ce6297` contains the implementation; bead close/reintegration was held until a cleaner beads window.

## Diff summary

- Code/content commits: `65d8ce6297` (local agent commit; final landed squash SHA will come from the reintegration receipt)
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched: `crates/caco-web/static/app.js`, `crates/caco-web/static/nodes.js`, `crates/caco-web/static/workspace-chat-pane.js`, `crates/caco-web/static/workspace-chat-pane.css`, `crates/caco-web/src/tests.rs`
- Tests: added static contract coverage for `bd-6d30f2`; no tests removed
- Behavioural delta: caco-web can now initiate group-scoped chat from node agent groups and deliver by direct message fanout instead of project broadcast or a new protocol.
- Validation: `node --check` on touched JS shards; `git diff --check`; `cargo test -p caco-web workspace_chat_pane_ -- --test-threads=2`; `cargo test -p caco-web app_js_nodes_view_has_master_detail_and_node_scoped_chat_bd_5f3d5c -- --test-threads=2`.

## Operator-takeaway

The slice deliberately uses the existing direct-message endpoint once per group member, so group chat inherits existing auth, feed, inbox, and delivery semantics. The biggest session surprise was that caco-web has both a standalone workspace chat shard and a separate integrated dashboard chat path; both were updated or guarded so the visible Nodes flow works.
