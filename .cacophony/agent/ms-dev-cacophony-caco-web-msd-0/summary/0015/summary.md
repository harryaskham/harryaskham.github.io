# Session summary — caco-web: render expected-offline nodes calmly (web half of bd-0e7c97)

## Goal

Implement the caco-web dashboard half of the offline-node-resilience work Harry
prompted (winmini/aurora/pocket4/sonance powered off by design while travelling).
The Nodes view previously rendered an intentionally-offline node identically to a
crashed one (alarming "down"/"unreachable"). With the daemon now exposing
expectation-aware node health (`peer_health`, landed via the sibling bd-03fad6),
this session renders an annotated offline-by-design node as a calm
"expected offline" while keeping genuine outages loud.

## Bead(s)

- `bd-0e7c97` — caco-web: Nodes view renders expected-offline nodes as alarming
  'down' (honor health_expectations.expected_unreachable). [web half implemented
  + validated this session]
- Daemon half landed separately as `bd-03fad6` (per-node expectation-aware health
  on `/api/v1/nodes`: `NodeSummary`/`NodeDetail` carry
  `peer_health: { status, expected, note }` derived from config
  `health_expectations` via `expects_status`).

## Before state

- Failing tests: none.
- `crates/caco-web/static/nodes.js` `nodeStatusClass()` mapped
  unreachable/down/offline → `down` with no expectation awareness; the only
  "expected offline" badge was codespace-specific (`describeCodespaceLifecycle`).
- Daemon `/api/v1/nodes` now includes `peer_health` (bd-03fad6) but nodes.js did
  not consume it. winmini config has `health_expectations.expected_unreachable:
  true` (+ note), yet rendered as a plain alarming "unreachable"/"down".

## After state

- Failing tests: none (static-only nodes.js change; gate runs cargo
  check/test-small/clippy on reintegration, unaffected by static assets).
- New `nodeLivenessBadge(info, isLocal)` helper in nodes.js: when a node's badge
  class is `down` AND `info.peer_health.expected` is true, it renders a calm
  `degraded` "expected offline" badge with `peer_health.note` as the tooltip;
  otherwise unchanged. `nodeStatusClass()` is intentionally left as the
  side-effect source (the "last known / not verified live" agent-count annotation
  still fires — the node genuinely IS unreachable, just by design).
- Used in both the subnav badge and the detail-panel badge; the detail panel also
  gains an "Expected" row showing the operator note.
- Validated end-to-end in chromium against the real bd-03fad6 data shape
  (injected, since the live daemon binary 1.2.1337 still predates bd-03fad6 and
  emits `peer_health: null`):
  - subnav: expected node → "expected offline"/degraded/note-tooltip; unexpected
    node → "unreachable"/down; no `peer_health` → unchanged "unreachable"/down
    (backward-compatible, no regression).
  - detail (direct `renderNodeDetail` render): "expected offline"/degraded badge +
    tooltip + an "Expected" row carrying the note.
  - JS syntax OK; nodes.js content-assertion tests (bd-0fe376 helper needles,
    bd-3760ac "expected offline") preserved.

## Diff summary

- Code/content commit: pending final squash SHA from the reintegration receipt.
- Files touched: `crates/caco-web/static/nodes.js` (+26/-2): add
  `nodeLivenessBadge` helper; use it for subnav + detail badges; add detail
  "Expected" row.
- Tests: +0 / -0 (static asset; validated via live browser probes).
- Behavioural delta: intentionally-offline nodes (config health_expectations,
  surfaced by daemon `peer_health.expected`) now read as a calm "expected
  offline" in the Nodes view instead of an alarming outage; real outages and
  un-annotated/pre-bd-03fad6 nodes are unchanged.

## Embedded artefacts

- `web/screenshots/nodes-expected-offline.png` — Nodes view with winmini (wmi)
  rendered "EXPECTED OFFLINE" (calm) while pocket4/aurora/sonance show
  "UNREACHABLE" (alarming), proving the distinction.

## Operator-takeaway

The dashboard now distinguishes offline-by-design from a real outage: an
annotated sleeping node (winmini) reads calmly as "expected offline" with its
operator note, while genuinely-unreachable nodes stay loud — directly addressing
Harry's "we should be resilient to offline nodes." This is the web half;
the daemon serialization (bd-03fad6) already landed, but the running daemon
binary on ms-dev still predates it (emits `peer_health: null`), so the calm
rendering activates fleet-wide only once daemons are rebuilt past bd-03fad6. The
change degrades gracefully until then (unchanged "unreachable" badges).
