# Session summary — bd-24540d msg snapshot agent filter fix

## Goal

Fix `caco msg snapshot --agent X` always returning 0 results,
discovered by test-user-hel during a surface-discovery sweep.

## Bead(s)

- `bd-24540d` — caco msg snapshot --agent always returns 0

## Before state

- `caco msg snapshot --project cacophony --agent <anything>` returned 0
  for every agent value, even when the inbox had 25 messages.
- Root cause: client-side filter only checked
  `target.contains(agent)`, but broadcasts and speaks have null/empty
  target — so they were universally filtered out.

## After state

- Filter now includes broadcasts (target is null/empty) since inbox
  already scoped delivery to caller, AND matches sender substring
  for direct messages where target is present.
- cargo test-small: 4299 pass; cargo clippy clean.

## Diff summary

- Commits: 5f0da3ff5e4c
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: 0 new (filter logic is downstream of network call; would
  require refactor to test in isolation — filed for follow-up if
  desired)
- Behavioural delta: `caco msg snapshot --agent X` now returns
  matching messages instead of always 0.

## Operator-takeaway

Inbox-style filtering on broadcasts requires recognising that
`target=null` means "everyone in scope" — the API already filtered
delivery, so the client-side narrow shouldn't drop them. Same shape
likely lurks in other inbox-derived surfaces; worth a sweep for
`target.contains` patterns.
