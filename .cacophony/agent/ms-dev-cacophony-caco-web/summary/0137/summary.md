# Session summary — bd-bf7c64: <noscript> fallback for JS-disabled users

## Goal
A11y/UX gap: 0 `<noscript>` elements across 4 entry HTML files. JS-disabled users saw blank app shell.

## Bead
- `bd-bf7c64`

## Fix
Added styled `<noscript>` block to all 4 entry HTML files:
- index.html (end-of-body, preserves bd-0de54e skip-link adjacency invariant)
- workspace.html, terminal.html, notifications.html (right after `<body>`)

Block content: `role="alert"` overlay with Nord-themed inline styles, headline "Cacophony dashboard requires JavaScript", 2 paragraphs explaining the failure, and a CLI hint suggesting `caco status` / `caco bd list` / `caco agent list` as alternatives.

## Debugging journey
First insertion in index.html broke the bd-0de54e invariant: skip-link must be IMMEDIATELY adjacent to `<body>`/comment with no whitespace text nodes (else absolute-positioned skip-link forces blank line above `#app`). Reverted and re-inserted at end-of-body for index.html only.

## Regression test (~30 lines)
- Iterates 4 entry HTML files.
- Asserts each contains `<noscript>...</noscript>`.
- Asserts block body mentions "javascript" (case-insensitive).
- Asserts block body length >200 chars (substantive guidance).

## Operator-visible effect
- JS-disabled users see clear message instead of blank page.
- Pattern in place for any new entry HTML file.

## Diff summary
- `crates/caco-web/static/{index,workspace,terminal,notifications}.html` -- added `<noscript>` block.
- `crates/caco-web/src/tests.rs` -- new regression test (~30 lines).
- Net pass: 554 -> 555; 0 failures.

## Operator-takeaway
45 cycles, 88 wins. Real a11y win for JS-disabled users + adjacency-invariant aware insertion.
