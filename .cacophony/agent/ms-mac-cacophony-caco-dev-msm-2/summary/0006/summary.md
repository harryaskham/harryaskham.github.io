# Session summary — Timeline view UI design (bd-db60a1)

## Goal

Land a contract document for a cross-surface (TUI / web /
Android) timeline view that displays project events
chronologically. Unblocks the implementation lane:
bd-734772 pipeline, bd-22cf2c caching, bd-8adcec TUI visual,
bd-5278e2/bd-ec4fdc/bd-cfe554 TUI views, bd-198dbd/bd-47dc20
Android, bd-fc8947 web.

## Bead(s)

- `bd-db60a1` — Design timeline view UI component

## Before state

- 12 sibling beads filed in the timeline lane with no shared
  contract — each surface would have re-derived event shape,
  filter semantics, and visual grammar independently.
- No agreement on event kinds, daemon endpoint shape, or scope
  semantics (per-project vs cluster).

## After state

- New `docs/epics/bd-db60a1-timeline-view-design.md` (~13.8KB)
  covers:
  - Goals/non-goals (read-only nav surface, not a viewer; no
    real-time push in v1; no AI summarisation in v1)
  - Single canonical event schema served at
    `GET /api/v1/projects/{project}/timeline` and
    `/api/v1/timeline` (cluster scope)
  - Six initial event kinds: commit_landed, release_tagged,
    bead_closed, bead_filed, agent_failed, reintegration —
    extensible without API churn
  - Query params (from / to / kinds / actor / limit / cursor)
    with bd-83a8ed `--since` parser reuse
  - Visual grammar: chip primitive (glyph + title +
    timestamp), 3 density tiers (TUI compact / TUI full+Android
    / web roomy), nord palette only, no new colors
  - Day-grouping with sticky headers
  - Click-through link semantics (bead / commit / release /
    agent / message)
  - Filter affordance (kinds / actor / range presets) — URL-
    state on web, in-memory on TUI/Android
  - Cluster scope: project badge with deterministic-hash color
  - Surface adaptation rules per-platform (TUI new
    `views/timeline.rs`, web new `workspace-timeline-pane.js`
    following bd-b9e32e log-pane pattern, Android
    `TimelineFragment`)
  - Performance targets (< 500ms cold, < 100ms warm) with
    caching strategy delegated to bd-22cf2c
  - Trade-off matrix and out-of-scope follow-ups
  - Acceptance map ticking each bd-db60a1 requirement

## Diff summary

- Files touched:
  - `docs/epics/bd-db60a1-timeline-view-design.md` (new)
- Tests: +0 / -0 / flipped 0 (design doc, no code)

## Operator-takeaway

Read before claiming any bead in the timeline lane (12 beads).
Five key decisions to review:

1. **Pull-only in v1, push deferred to bd-734772 pipeline** —
   surfaces poll on mount + every 60s. Avoids over-coupling the
   contract to SSE infrastructure that's still being built.
2. **Six event kinds locked in** — daemon-side enumerator must
   only emit these until a follow-up bead extends the registry.
   New kinds = additive, no breaking changes.
3. **Single envelope, all surfaces** — start minimal; add
   optional fields under `#[serde(default)]` rather than
   per-surface variants. Keeps the daemon contract honest.
4. **Cluster scope opt-in** — surfaces start in per-project
   scope; operator flips to cluster via toggle (TUI `Tab`, web
   breadcrumb, Android segmented control).
5. **Reuse existing primitives** — nord palette only, existing
   icons, follow the bd-b9e32e workspace-log-pane.js mount
   pattern for web. No new design tokens.

If caco-ctrl wants to flip any of these, this doc is the cheap
place to do it before bd-734772 / bd-8adcec land.

This is the second design doc landed this session (after
bd-1975be Codespaces architecture). Both follow the same
shape: 11-12 sections, acceptance-map at end, trade-off matrix.
The pattern is settling — future design beads should adopt it.
