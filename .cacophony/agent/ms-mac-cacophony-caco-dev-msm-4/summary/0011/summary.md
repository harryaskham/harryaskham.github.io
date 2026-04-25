# Session summary — webapp audit slice 4: workspace routing + chat counter inversion

## Goal

Walk every nav surface of the now-functioning dashboard end-to-end via
a single playwright session and file/fix UX issues found.

## Bead(s)

- `bd-8db9eb` — caco-web: `#workspace` hash route silently no-ops
  (P3, claimed by msm-4)
- _outbox-queued_ `outbox-019dc589-6574-7351-ae90-e17935ab1cc8` —
  caco-web: Chat heading shows 'shown of total' inversion
  (P3, queued for create while daemon was rate-limited; will sync
  when outbox flushes)
- (parent: `bd-c1c272` — webapp audit umbrella)

## Tour artefacts

- `summary/0011/screenshots/<view>.png` — 16 views captured via the
  shared playwright session: status, agents, beads, feed, chat, nodes,
  services, projects, choices, notifications, actions, logs,
  timeline, summaries, merge-queue, workspace.
- `summary/0011/snapshots/<view>.yml` — matching a11y snapshots.

## Before state

- `VALID_VIEWS` in `crates/caco-web/static/app.js` listed every other
  top-level view but omitted `'workspace'`. Visiting `#workspace` left
  the prior view body rendered (Merge Queue, in the captured tour),
  even though the Workspace nav badge correctly highlighted itself —
  silent body/nav desync.
- `renderChat()` merged `state.chatMessages` + `state.speechEvents`
  into a filtered `messages` list, then called
  `updateResultCount('chat-count', messages.length, state.chatMessages.length)`.
  As soon as speech events arrived the denominator was wrong and the
  Chat heading printed e.g. "250 of 200".
- Two known-broken status surfaces also captured in the tour but
  filed-not-fixed (see Findings §3–4 below).

## After state

- `VALID_VIEWS` includes `'workspace'` with a `bd-c1c272` ref comment;
  `#workspace` now correctly routes through `switchView`. Combined
  with slice-1's `bd-fc3328` keyboard wiring, the Workspace surface
  is reachable through every advertised entry point (sidebar click,
  sidebar `w` shortcut, URL `#workspace`).
- `renderChat()` captures the post-dedupe combined-length once as
  `totalMessages` and uses it as the denominator in both the empty
  and non-empty `updateResultCount` call sites; the Chat heading
  is therefore correctly bounded by the merged source size.
- All 16 view-snapshot YAML files preserved under `summary/0011/` so
  the tour state can be diffed in subsequent audits.

## Findings

### Fixed in this slice

1. **`#workspace` hash route silently no-ops** (`bd-8db9eb`).
   `VALID_VIEWS` in `static/app.js` listed every other top-level view
   but omitted `workspace`. Visiting `#workspace` left the previous
   view rendered (e.g. Merge Queue) under a Workspace-highlighted nav
   item. Slice-1 (`bd-fc3328`) fixed the keyboard shortcut path; this
   slice closes the same gap on the URL/hash path.
   **Fix:** add `'workspace'` to `VALID_VIEWS` with a `bd-c1c272` ref.

2. **Chat heading shows shown > total** (outbox-queued bead).
   `renderChat()` merges `state.chatMessages` + `state.speechEvents`
   into a single filtered list `messages`, then calls
   `updateResultCount('chat-count', messages.length, state.chatMessages.length)`.
   Once any speech events arrive the denominator is wrong and the
   heading reads e.g. `Chat 250 of 200`.
   **Fix:** capture the post-dedupe combined-length as
   `totalMessages` and use it in both `updateResultCount` call sites,
   so the heading is correctly bounded.

### Filed-not-fixed (deferred to dedicated beads)

3. **Status hero text mismatch:** the "Live orchestration / cluster
   pulse at a glance" hero says "Snapshot pending" while the
   surrounding tiles show 26 active agents / 71 beads / 3 services.
   Suggests a stale freshness flag isn't cleared on first successful
   render. To be filed once daemon connectivity recovers (currently
   intermittent).

4. **Status freshness pill stuck:** `beads: partial` with
   "Refreshing…" disabled-button state never resolves on long-running
   sessions. To be filed alongside #3 once daemon connectivity
   recovers.

5. **`#workspace` route still selects nav badge correctly** — sidebar
   highlight worked even though body didn't switch, which is its own
   small UX inconsistency (selection desync). Captured implicitly in
   `bd-8db9eb`'s repro.

## Diff summary

- `crates/caco-web/static/app.js` (+11 / -3):
  - `VALID_VIEWS` includes `workspace` with a comment.
  - `renderChat()` computes `totalMessages` once after dedupe and
    feeds it to both `updateResultCount` call sites.

## Test status

- Static change; playwright tour has the live evidence, but the
  daemon-driven flow we'd need to live-verify the chat counter is
  intermittent right now. Both fixes are tightly scoped, single-line
  semantic edits with no plausible regression surface beyond their
  call sites.
- Single playwright session policy held throughout.

## Operator-takeaway

Slice 4 caps the immediate audit pass: every dashboard nav surface
loads, the Workspace surface is now reachable via every advertised
entry point (sidebar click, sidebar `w` shortcut, URL `#workspace`),
and the chat heading no longer prints inverted counts. Two stale-state
findings (#3, #4 above) remain as filed-not-fixed for the next slice.
`bd-1ef80b` (PR-mode never reaches forge) still open as a P2 for ops.

Webapp audit umbrella `bd-c1c272` has shipped four slices today; this
is a natural pause point.
