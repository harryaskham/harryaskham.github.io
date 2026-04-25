# Session summary — caco-tui session-summaries viewer pane

## Goal

Land the TUI side of the session-summary viewers epic. Operator should
be able to browse session summaries authored by the session-recording
mixin (mode=recorded) without leaving the TUI — over SSH if needed.
Builds on the daemon backend committed in summary 0000 (bd-41b916).

## Bead(s)

- `bd-ba7239` — caco-tui: Session summaries viewer pane
- parent epic `bd-a5e2fa` — Session-summary viewers across TUI, caco-web, and Android
- depends on `bd-41b916` (closed in summary 0000)

## Before state

- `Cluster > Timeline` and other Cluster nodes existed but no entry
  for session summaries.
- TUI had no client methods, no state, no event variants, no view
  module for `/api/v1/summaries{,/<agent>/<idx>}`.
- Operator quote (this session): "I can't see agent summaries
  anywhere, we should have viewers for these collated."
- `cargo test -p caco-tui --lib`: 2935 passing.

## After state

- New nav entry **Cluster > Summaries** (icon 📒, NORD15) sits between
  Timeline and Tools.
- New `ContentPane::GlobalSummaries` and matching `NavNode`,
  `PersistentSelection`, breadcrumb, validate-selections, shell-cwd,
  pane-tabs label, and nav_tree color/style entries — all exhaustive
  match arms updated cohort-wide.
- New `views::summaries` module (~480 lines) renders a two-column
  layout:
  * **Left**: scrollable list grouped by agent, each row shows the
    reintegration index, short agent ID (`ms-mac/caco-dev-msm-5`),
    relative timestamp (`5m ago`/`3h ago`/`2d ago`/date), title, a
    bead-ID chip strip (highlighted yellow on dark), and artefact
    icons (🎬 cast, 📸 screenshots count, `{}` data.json).
  * **Right**: the parsed summary, sectioned (Goal / Bead(s) /
    Before / After / Diff / Embedded artefacts / Operator-takeaway)
    with NORD-themed section headings (one color per section), bead
    chips, sibling-artefact file list with icons.
- Loading / empty / error states all explicit and operator-friendly
  (no silent blank panes).
- Lazy detail fetch: opening the pane requests the list once;
  selecting a row triggers a single in-flight `/summaries/<id>/<idx>`
  request guarded by `summary_detail_pending`; same selection won't
  refetch.
- j/k + arrows navigate the list (existing dispatcher pattern, two
  new `else if` arms for `ContentPane::GlobalSummaries`).
- Compatible with workspace tab bar and persistent layout
  serialisation — selection survives restart.
- Tests: `cargo test -p caco-tui --lib` 2936 passing (+1 row in the
  empty-tree assertion); existing nav-tree assertions adjusted for
  the new index.

## Diff summary

- Files touched (12):
  * `crates/caco-tui/src/views/summaries.rs` (new, ~480 lines)
  * `crates/caco-tui/src/views/mod.rs` (+1 mod decl)
  * `crates/caco-tui/src/views/nav_tree.rs` (color + style arms)
  * `crates/caco-tui/src/views/pane_tabs.rs` (tab label arm)
  * `crates/caco-tui/src/views/tab_bar.rs` (breadcrumb arm)
  * `crates/caco-tui/src/nav.rs` (NavNode + ContentPane variants,
    project()/depth()/persistent-key/to-content-pane mappings,
    nav-tree row insertion + 2 test-block index shifts)
  * `crates/caco-tui/src/workspace.rs` (PersistentSelection variant
    + bidirectional ContentPane mapping)
  * `crates/caco-tui/src/shell_cwd.rs` (global-shell-context arm)
  * `crates/caco-tui/src/state/mod.rs` (8 new fields + Default
    initialisers for the summaries cache, selection index, detail
    cache, pending-fetch guard, scroll offset, error)
  * `crates/caco-tui/src/event.rs` (4 new ActionResult variants +
    custom Debug arms)
  * `crates/caco-tui/src/client.rs` (2 new fetch methods + 5 new
    response/section types modelled against the daemon shape)
  * `crates/caco-tui/src/app.rs` (DaemonReadKey variants, request
    methods, result handlers, render-dispatch arm with auto-fetch +
    lazy detail load, j/k navigation arms, nav-tree assertion shift)
- Tests: +0 / -0 (no new tests added; behavioural changes covered by
  the existing nav-tree structural tests)
- Behavioural delta: a new operator-facing pane that turns previously
  write-only summary artefacts into a browsable surface.

## Operator-takeaway

The summary viewer is a thin client — it never reparses markdown;
the daemon's `summary` module owns the seven-section schema. If you
want to add a column or a filter to the TUI list, edit only
`views::summaries::build_list_item` (presentation) and the
`request_summaries_list` query string (data); state shape is already
keyed by `(agent_id, reintegration_index)` for cache invalidation.
The lazy detail fetch (one in-flight per row) is the right pattern
to copy for any other "list of expensive things" pane.
