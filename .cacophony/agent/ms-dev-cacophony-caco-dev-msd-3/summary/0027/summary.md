# Session summary 0027 — bd-ca10e9: caco-web/static README + discovered missing <link> tags

## Goal

Land the missing index for caco-web's CSS shards so future
contributors don't reverse-engineer ownership from grep.

## Bead(s)

- `bd-ca10e9` — webapp-docs: add crates/caco-web/static/README.md indexing the 12 CSS shards
- `bd-d59265` — **filed** P1 bug discovered during indexing (5 workspace-* CSS shards embedded but never linked from workspace.html)

## Before state

- 9 .css files in `crates/caco-web/static/` (bead said 12; 9 actual).
- No README; ownership and load order had to be reverse-engineered.
- workspace.html `<link>` tags missing for 5 of the 7 workspace shards
  (latent bug; nobody had noticed).

## After state

- New `crates/caco-web/static/README.md` (132 lines):
  - shard inventory table with owner-bead-id, where loaded, scope
  - design-token contract (Nord palette + workspace overrides)
  - recommended load order (vendor → tokens → a11y → view shards)
  - "where to add new view-scoped CSS" walkthrough
  - known-issues section calling out the discovered bug
- Filed `bd-d59265` P1 bug with full reproducer + AC for both
  add-link-tags and lazy-load fix shapes plus an end-to-end pin
  test.

## Diff summary

- `crates/caco-web/static/README.md`: new file, 132 lines.
- No code changes; no tests required.

## Embedded artefacts

(none)

## Operator-takeaway

Two wins for the price of one bead: the documentation lands AND
flushes out a P1 latent bug that's been silently shipping unstyled
panes since the workspace MVP. The bd-d59265 fix is small (5 link
tags + 1 pin test) and unblocks the workspace-view V2 work the
operator just queued.

bd-ca10e9 acceptance is fully met (README written, indexed, design-
token contract documented, load-order documented, where-to-add-new
walkthrough documented). Will close after reintegrate per audit.
