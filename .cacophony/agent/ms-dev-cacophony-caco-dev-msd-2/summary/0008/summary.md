# Session summary 0008 — bd-9c3fac caco bd graph

## Goal

Land bd-9c3fac: a `caco bd graph` subcommand that renders the
bead-relationship graph (parent/child + dependencies) as DOT
(graphviz), mermaid (markdown-friendly), or terminal ASCII. The
operator filed this after building the bead-graph cluster
(bd-c5c3a0 labels, bd-d8fc57 parent-id, bd-a23a7e
occurrence-count, bd-05b1f8 dedup, ...) and noting there was no
visual way to see how those pieces relate.

## Bead(s)

- `bd-9c3fac` — primary, claimed and worked.

## Before state

No bd graph subcommand. Operators had to read JSONL and trace
parent_bead_id / dependencies by eye to understand cluster shape.

## After state

`caco bd graph [--root bd-XXX] [--depth N] [--format dot|mermaid|ascii]
[--status open] [--include-closed]` produces a status-coloured
graph. Validated end-to-end against the live cacophony project:
all three formats render; bd-list-style filtering applies
client-side; BFS scoping works for both edge kinds undirected.

## Diff summary

`crates/caco-cli/src/lib.rs` (+545):

- New `BD_GRAPH_ARGS` ArgSpec block (project, root, depth, format,
  status, include-closed).
- New `CommandSpec` entry under `BD_SUBCOMMANDS` for `graph`
  (mcp_enabled, agent_safe, idempotent).
- New dispatcher arm: `[cmd, sub] if cmd == "bd" && sub == "graph"
  => return dispatch_bd_graph(...)`.
- New `dispatch_bd_graph(json, flags, multi_flags, config)`
  function that resolves project + token + node, validates
  `--format` and `--depth`, fetches `GET
  /api/v1/projects/<p>/beads?limit=2000&status=all`, parses each
  bead via `graph_node_from_value`, applies status filter +
  closed-skip, then calls `render_bead_graph(...)`. JSON mode
  wraps the rendered string in the standard ok envelope.
- New `GraphNode` struct: id, title, status, parent_bead_id,
  dependencies.
- New `graph_node_from_value(&serde_json::Value) -> Option<GraphNode>`
  pulling the relevant fields out of the daemon's bead JSON, with
  graceful defaults for missing optionals.
- New `render_bead_graph(nodes, root, depth, format) -> String`
  that scopes via `bfs_reachable` when `root` is set, then
  dispatches to the per-format renderer.
- New `bfs_reachable(nodes, root, depth)`: walks parent +
  dependency edges as an undirected graph from `root` up to
  optional `depth` hops, returns the visited set.
- New `graph_status_color(status) -> &'static str`: monokai-style
  per-status fillcolors (open green, in_progress blue, closed
  grey, draft yellow, permanent purple, fallback off-white).
- New `render_dot(nodes, visible)`: digraph beads { ... }
  rankdir=LR, monospace fontname, status fillcolor, parent-of
  edges in grey, blocks edges in red, label-escape backslashes
  + double-quotes.
- New `render_mermaid(nodes, visible)`: graph LR with `_`
  IDs (mermaid forbids `-`), per-status `classDef` blocks,
  `-.parent.->` and `==blocks==>` edges.
- New `render_ascii(nodes, visible)`: status-bracketed lines
  ([open], (closed), {draft}, <permanent>) with parent-of and
  blocked-by follow-up lines indented under each node.

10 new tests under `bd_graph_tests` mod:
- `graph_node_from_value_extracts_fields`
- `graph_node_from_value_handles_missing_optionals`
- `bfs_reachable_unbounded_walks_both_edge_kinds`
- `bfs_reachable_depth_bounded_stops_walking`
- `bfs_reachable_unknown_root_returns_empty`
- `render_dot_contains_node_and_edge_labels`
- `render_mermaid_uses_underscore_ids_and_classdefs`
- `render_ascii_uses_status_brackets_and_edges`
- `render_bead_graph_dispatches_on_format`
- `graph_status_color_known_and_fallback`

`crates/caco-tui/src/app.rs` (+8): drive-by sweep — 4 more
SessionKickedModal sites needed `tmux_history_limit: None,
tmux_history_size: None,`. Same Python brace-walker as previous
sessions, one iteration sufficed. msm-5's WIP keeps reverting the
struct definition extension; this is a recurring keep-the-tree-
green pattern.

## Files touched

- `crates/caco-cli/src/lib.rs` (+545)
- `crates/caco-tui/src/app.rs` (+8)

Total: 2 files, +553.

## Operator-takeaway

`caco bd graph --format mermaid > beads.md` produces a
markdown-pasteable view of every open relationship in the
project. `caco bd graph --root bd-c5c3a0 --depth 2 --format dot
| dot -Tpng -o cluster.png` produces a PNG focused on a single
cluster. `caco bd graph` with no flags gives a quick ASCII
status summary showing what is parented to what and what blocks
what — useful for triage cluster reviews.

Renderers are pure functions over `&[GraphNode]` so they're
trivially unit-testable; the dispatcher is the only piece that
touches the daemon.

Future hooks (free as the underlying fields land):
- duplicate-of edges (bd-517e52) will appear automatically once
  the `duplicate_of` field is added to the Bead model — extend
  `graph_node_from_value` to read it and `render_*` to draw it.
- composes-with, blocked-by-as-of, label-cluster groupings, etc.

## Validation

- `cargo build --bin caco`: clean.
- `cargo test -p caco-cli --lib bd_graph_tests`: 10 / 10 PASS.
- `cargo test-small`: 212 + 109 + 739 + 295 + 18 + 2819 + 56
  PASS.
- `cargo clippy --workspace --all-targets -- -D warnings`: clean
  (17.77s).
- Live smoke: `caco bd graph --project cacophony` renders 314
  open + in_progress + draft + permanent beads as ASCII;
  `--format mermaid` and `--format dot` both produce well-formed
  output; `--root <id> --depth N` correctly scopes.

## Notes / follow-ups

- The dispatcher uses `limit=2000&status=all` to get the full
  corpus; if bead counts blow past 2000 in some project we'll
  need pagination or a server-side `/graph` endpoint.
- `--edges parent|deps|all` would be a one-line follow-up if
  operators want to focus on just one edge kind.
- The mermaid rendering produces enough nodes to choke
  livepreview on very large projects; a `--max-nodes N` flag
  could be added if that becomes painful in practice.
