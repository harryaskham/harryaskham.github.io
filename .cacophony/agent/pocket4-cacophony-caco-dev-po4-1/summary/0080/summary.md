# Session summary — Trust CLI surface guidance in dev profile

## Goal

Add the operator's "trust the CLI, don't invent regulation" guidance to
the dev profile's Rules section, complementing the common_instructions
addition from bd-3ae61c.

## Bead(s)

- `bd-29f9a0` — [profile] trust beads CLI surface; fix blockers instead of inventing regulation loops

## Before state

- common_instructions.txt already had the "Trust the CLI surface" guidance from bd-3ae61c
- dev.md Rules section did not mention it

## After state

- dev.md Rules section now includes the trust-CLI-surface rule

## Diff summary

- Commits: d89e21f04
- Files touched: `.cacophony/profiles/dev.md`
- Tests: no changes
- Behavioural delta: dev workers now see the trust-CLI guidance in their
  profile-specific rules, not just the common layer.

## Operator-takeaway

Small profile polish that ensures the operator's "trust the CLI, fix
blockers" guidance appears in both the common instruction layer and the
dev profile's rules section where workers look for do/don't guidance.
