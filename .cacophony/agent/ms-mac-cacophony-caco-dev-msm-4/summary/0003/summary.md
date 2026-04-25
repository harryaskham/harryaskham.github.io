# Session summary — bd-a15cd9 macOS Status actionability

## Goal

Make the native macOS Status pane faster to interpret by turning it from a passive dashboard into a triage surface with clear stale/degraded cues and operator next-step wording.

## Bead(s)

- `bd-a15cd9` — [macOS excellence] Status overview actionability polish

## Before state

- The Status pane already showed project scope, metrics, an operator summary, recommendations, node details, and projects.
- Stale/degraded live stream and restart-pending states were not elevated before the normal choice/queue/fleet messages, so operators had to infer whether counts were safe to act on.
- The node card was mostly raw identity fields without a clear cue about snapshot freshness or configuration drift.

## After state

- The top severity readout now surfaces restart-pending and live-stream degraded/stale states before normal operational summaries.
- A new “What to do next” panel gives one prioritized action row with concrete pane guidance for restart drift, stale stream, attention notifications, choices, active automation, ready work, or calm state.
- The node section now includes a node-health cue and exposes config hash alongside name/version/local/cluster addresses.

## Diff summary

- Commits: pending reintegration commit for `bd-a15cd9`.
- Files touched: `companion/macos/Sources/Cacophony/Views/StatusPane.swift`.
- Tests: `just macos-app-test`; `./docs/validate-pages.sh`; `git diff --check`.
- Behavioural delta: macOS operators get explicit next-step wording and freshness warnings directly on Status before taking queue/fleet actions.

## Operator-takeaway

The Status pane now answers “what should I do first?” instead of only showing counts: stale stream and restart drift are front-loaded, and calm/attention/automation states each point to the correct follow-up pane.
