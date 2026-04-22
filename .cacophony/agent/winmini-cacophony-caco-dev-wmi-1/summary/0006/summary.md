# Session summary — caco-web scrollback usage in agent detail (bd-7ab1b9)

## Goal

bd-7ab1b9 (follow-up to bd-7ef076 / bd-87f5bf): surface
`tmux_history_limit` + `tmux_history_size` in the caco-web Agent
Detail "Info" tab so operators can see how close a long-running
agent is to the 100k scrollback ceiling.

## Bead(s)

- `bd-7ab1b9` — caco-web: render tmux_history_limit +
  tmux_history_size in agent detail
- (parent: `bd-7ef076` — daemon slice that added the fields to
  `/api/v1/projects/{project}/agents/{agent_id}`; closed)
- (grandparent: `bd-87f5bf` — raised AGENT_TMUX_HISTORY_LIMIT to
  100k)

## Before state

- Failing tests: none. caco-web::tests: 52 passing.
- The fields were available on the per-agent endpoint but the
  caco-web Info tab read only from the snapshot's compact agent
  record, which doesn't include the live tmux probe. Operators
  had no visual cue when an agent was approaching the scrollback
  ceiling.

## After state

- Failing tests: none. +1 test (53 total).
- Info tab gains a "Scrollback" section with formatted line
  counts (size / limit + percentage) and a thin coloured progress
  bar (green <60%, yellow <90%, red ≥90%).
- Section is skipped on terminal agents (completed / discarded /
  failed / stopped) since their tmux is gone.
- "Loading…" placeholder while the fetch is in flight; "Not
  available" notice when the daemon's tmux probe returned empty.
- Fetch is coalesced via a monotonically-increasing
  `scrollbackFetchId` so rapid tab switches don't stampede or
  clobber a fresher response.

## Diff summary

- Files touched:
  - `crates/caco-web/static/app.js` (+~95): new
    `renderAgentScrollbackSection` + `fetchAgentScrollback`
    helpers, `agentDetailState.scrollback` cache + fetchId
    coalescing, integration into `renderAgentInfoTab`.
  - `crates/caco-web/static/style.css` (+15): `.agent-scrollback-bar`
    + fill rules with a 200ms eased transition.
  - `crates/caco-web/src/tests.rs` (+~50): pin the wire path,
    helper names, daemon-side field reads, and CSS hook in one
    characterisation test.
- Tests: +1 / -0 / flipped 0
- Behavioural delta:
    - Operators viewing a running agent now see live scrollback
      usage that refreshes on every detail-tab visit, with a
      coloured cue (yellow/red) when they're approaching the cap.
    - No change for terminal agents.

## Embedded artefacts

(none — frontend slice only; daemon slice was already shipped in
bd-7ef076)

## Operator-takeaway

Open Agent Detail → Info tab on any running agent. New
"Scrollback" section appears just below "Reintegration" showing:

    Lines: 12,345 / 100,000 (12.3%)
    [▓▓▓░░░░░░░░░░░░░░░░░░░░░░░░] (green)

Bar turns yellow at 60% usage and red at 90%. If you see red, the
agent is within ~10k lines of the bd-87f5bf cap and may want to be
reintegrated soon to avoid losing earliest scrollback.

Twins still open for the same scrollback-surface story:
- bd-b69cf3 (caco-tui)
- bd-290559 (android)
