# Session summary — bd-5bfb2c (slice 18)

## Goal
Line numbers, bead tags/deps, nav badge, welcome experience.

## Bead(s)
- **bd-5bfb2c** (P0 PERMANENT): workspace-view DO-OVER

## Before state
- Source viewer had no line numbers
- Bead detail didn't show tags or dependencies
- No indicator on sidebar when choices pending
- First-time users got empty single pane

## After state
- Source: synced line number gutter
- Bead detail: tags as pills, depends_on + linked_beads as code links
- Nav badge: red pill with count, pop animation
- Welcome: first-run gets Mission Control preset
- 245/245 tests (4 new)

## Diff summary
- workspace-panes.js: line number gutter, bead tags/deps rendering
- workspace-integrated.js: nav badge update, welcome first-run logic
- style.css: gutter, tags, deps, badge CSS
- tests.rs: 4 new tests

## Operator-takeaway
Source viewer now has line numbers. Bead detail shows tags + dependencies. Red badge appears on sidebar when choices need attention. First-time visitors land on Mission Control.
