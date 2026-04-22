# Session summary — bd-5d53b7 TUI claims attribute to operator

## Goal

Direct CLI/TUI bead claims should attribute to the operator, not
`{node}:tui`. The CLI was already correct; the TUI was bypassing
SPEC 9.4 caller-ID format and producing 2-segment caller IDs that
the daemon's `normalize_assignee_for_agents` then left untouched.

## Bead(s)

- `bd-5d53b7` — Direct CLI/TUI claims should attribute to operator,
  not node+tui (P2 feature)

## Before state

- `crates/caco-tui/src/app.rs` set `client.set_caller(format!("{}:tui",
  snapshot.node.name))` on both initial snapshot fetch and SSE
  reconnect.
- `normalize_assignee_for_agents` (caco-daemon::beads) only strips
  the node prefix for callers with ≥3 segments. Two-segment callers
  pass through unchanged.
- Result: a `bd claim` from alice's TUI on `ms-mac` was stored as
  assignee `ms-mac:tui` rather than `cacophony:alice`. Bead-burndown
  views, audit logs, per-operator stats could not distinguish
  operators sharing a node, nor distinguish operator activity from
  automated `mode:*` claims.
- Failing tests: none (latent bug — test pinned the wrong literal).

## After state

- New `caco-tui::app::build_tui_caller(snapshot_node, default_project)`
  helper produces a SPEC 9.4 `{node}:{project}:{actor}` triple with
  the same priority chain as the CLI's
  `infer_caller_with_context`:
  - node: `CACO_NODE` env > `snapshot.node.name` > `"localhost"`
  - project: `CACOPHONY_PROJECT` env > `snapshot.default_project` >
    `"unknown"`
  - actor: `CACOPHONY_ACTOR` env > `USER` env > `"tui"` (preserves
    the historical fallback so unattended demo TUIs without an
    operator env still produce a parseable triple while preserving a
    clear "no operator inferred" signal)
- Pure inner form `build_tui_caller_pure` extracted so unit tests
  can assert the priority chain hermetically without mutating
  shared process env.
- Wired at both call sites in `app.rs`: `ActionResult::Reconnected`
  and `ActionResult::SnapshotFetched`.
- 6 new unit tests; legacy `snapshot_fetched_applies_state` test
  updated to assert the structural 3-segment invariant (with
  `CACO_NODE` tolerance) instead of the literal `mynode:tui`.
- `cargo test-small` green (45 base + 2787 caco-tui unit tests).
- Failing tests: none.

## Diff summary

- Commit: `1d04d67d`
- Files touched:
  - `crates/caco-tui/src/app.rs`: +195 / −6
    - new `build_tui_caller` + `build_tui_caller_pure` helpers
    - two call sites updated (Reconnected, SnapshotFetched)
    - 6 new unit tests + 1 existing test relaxed to structural form
- Behavioural delta: TUI bead claims are now attributable to the
  operator (`{project}:{actor}` after daemon-side node-prefix
  stripping). Operators sharing a node are now distinguishable in
  burndown / audit views; TUI claims are now distinguishable from
  `mode:*` automated claims.

## Operator-takeaway

A direct `bd claim` from the operator's TUI now stores the operator
identity as the assignee (matching the CLI behaviour that has been
correct since bd-fff055). No migration is needed for already-claimed
beads — only newly claimed ones pick up the corrected attribution.
If an operator wants to run their TUI under an explicit identity
(e.g. for shared workstations or service accounts) they can export
`CACOPHONY_ACTOR` and the TUI will use that in preference to `USER`.

The historical 2-segment `{node}:tui` form is no longer emitted by
the TUI for the new claim path. Other code paths in the daemon /
CLI that hard-code `{node}:tui` (test fixtures, benchmark support)
are left untouched as they are not on the operator-attribution
path; they remain valid 2-segment caller IDs that the daemon
recognises as non-agent.

## Embedded artefacts

(none)
