# Session summary — bd-7ef076 daemon-side tmux scrollback metrics

## Goal

bd-87f5bf raised the per-agent tmux history-limit from the
default 2000 lines to 100 000. Operators have no visibility on
how much of that ceiling a long-running agent is actually
consuming. This bead adds the daemon-side surface so viewers
(TUI / web / android) can render a "scrollback used / limit"
indicator without each surface having to invoke tmux on its own.

## Bead(s)

- `bd-7ef076` — TUI/web/android: surface tmux history-limit
  value and bytes-used in agent detail (daemon slice in this
  session; viewer follow-ups filed).

## Diff summary

`crates/caco-daemon/src/agent/health.rs`:
- New `pub fn query_tmux_history_size(socket, session) -> Option<u32>`
  runs `tmux display-message -p -t <session> '#{history_size}'`
  against the per-agent socket. Returns `None` on any failure
  (no server, missing session, parse error) so callers can
  degrade gracefully — read-only probe, safe to call from
  per-agent detail handlers but explicitly NOT recommended
  inside list-loops over the whole fleet (one tmux exec per
  call).
- Pure parser `parse_tmux_history_size_output` split out so the
  stdout-handling has unit-test coverage without invoking tmux.
- 3 new unit tests:
  - integer parse with/without whitespace
  - rejects empty / non-numeric / negative / mixed input
  - asserts `AGENT_TMUX_HISTORY_LIMIT == 100_000` to keep the
    per-pane ceiling explicit.

`crates/caco-daemon/src/lib.rs`:
- `handle_project_agent_show` now mutates the JSON `data` blob
  before enveloping it: injects
  - `tmux_history_limit: AGENT_TMUX_HISTORY_LIMIT`
  - `tmux_history_size: <live u32>` (only when the live tmux
    query succeeds; absent otherwise so viewers can render
    "limit known, size unknown" gracefully).
  Skipped when `info.tmux_session` is empty.

`cacophony` beads filed as follow-ups for the viewer slices:
- `bd-7ab1b9` — caco-web agent detail
- `bd-290559` — companion Android agent detail
- `bd-b69cf3` — caco-tui agent detail

## Before state

- `GET /api/v1/projects/{project}/agents/{agent_id}` returned
  `AgentInfo` only; no scrollback metrics anywhere on the API.
- Operators had to know about the constant in `health.rs` and
  manually run `tmux display-message #{history_size}` against
  the per-agent socket to see usage.
- msd-4 just landed `caco agent log` (bd-83a84d) which gives
  raw scrollback access via tmux capture-pane; this bead's
  metrics are the headline counter that pairs with it.

## After state

- Per-agent detail JSON includes both ceiling and live size
  (when tmux can answer).
- Three follow-up beads file the per-surface viewer work:
  caco-web, Android, caco-tui — each is a small bind-and-render
  job that reads the new fields with no daemon work needed.

## Out of scope

- Bytes-used estimate. The original bead description suggested
  `line_count * average_line_length` or `/proc` inspection.
  Both are noisier than the line-count signal; deferred until a
  surface actually wants to render bytes.
- Including the metrics on `GET /api/v1/agents` (the fleet
  list). One tmux exec per agent on every list call would
  swamp the daemon under fleets like the current 157-agent
  inventory. Detail-only is the right scope.

## Operator-takeaway

Per-agent JSON now surfaces tmux scrollback usage:
`tmux_history_limit` (constant 100k) and `tmux_history_size`
(live count from `#{history_size}`). Three follow-up beads
filed for the viewer surfaces (caco-web bd-7ab1b9, android
bd-290559, caco-tui bd-b69cf3). Pairs with msd-4's bd-83a84d
(`caco agent log`) which exposes the raw scrollback content.
