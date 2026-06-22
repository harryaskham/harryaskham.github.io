# Session summary — bd-0d830a: preload entry workspace.js

## Goal
Pattern (m) preload entry-script parity: workspace.html lacked preload for /workspace.js (index.html already preloads /app.js).

## Bead
- `bd-0d830a`

## Audit
- grep `rel="preload"` across HTML.
- Only index.html (1 preload). workspace.html missing.

## Fix
- workspace.html head: + `<link rel="preload" as="script" href="/workspace.js">`.

## Why
- defer scripts only start fetch when HTML parser reaches the tag.
- preload in head starts fetch as soon as head is parsed.
- Saves 50-200ms on slow connections for complex workspace.html (~10 scripts).

## Regression test (~30 lines)
- Per (file, href) tuple, literal preload string assertion.

## Operator-visible effect
- Faster workspace bootstrap on slow networks.

## Diff summary
- `crates/caco-web/static/workspace.html` -- preload link + 3-line comment.
- `crates/caco-web/src/tests.rs` -- new bd-0d830a forward-guard (~30 lines).
- Net pass: 585 -> 586; 0 failures.

## Operator-takeaway
77 cycles, 120 wins. Pattern (m) preload parity. Pattern catalog: 22 entries.
