# Session summary — caco-profile self-improvement: 3 durable Workflow Gotchas

## Goal

Per Harry's directive to convert heavy useful session context into durable self-improvements before compacting, capture three genuinely-durable workflow learnings from this long session into the caco-profile profile's Workflow Gotchas so future caco-profile agents benefit.

## Bead(s)

- No bead (self-improvement, sanctioned by Harry's explicit "make profile fixes" directive; the self-improvement mixin is composed).

## Before state

- `caco-profile.md` Workflow Gotchas had 3 bullets (plugin-wrapper regen, intentional persistent-overlaps, queued-suite validation). None captured this session's durable learnings on land-verification, the non-caco-dev echo-gate Rust risk, or doc-thrashing prevention.

## After state

- Added 3 concise, actionable gotchas: (1) verify land durability against TRUE GitHub (gh-api compare / ssh fetch, never HTTPS; failed fetch = inconclusive, not a revert); (2) caco-profile is non-caco-dev so the echo/`--skip-hooks` path won't catch Rust breaks — doc-only lands are fine, but caco-profile crate Rust changes are workspace-wide release-blocking and need self-validation (and ms-mac queued-cargo caveat); (3) anticipate doc-thrashing on fleet-wide operator announcements — verify current state + check for in-flight owners, HOLD+flag rather than race, route durable prose through technical-writer.
- `docs/profiles.html` unchanged (body-only edit, description untouched) — `--check` up-to-date. Lifecycle audit clean (83/0).

## Diff summary

- Code/content commit: one commit; final landed squash SHA from the reintegration receipt.
- Summary artefact commit: intentionally omitted (no self-reference).
- Files touched: `.cacophony/profiles/caco-profile.md` (Workflow Gotchas, +3 bullets).
- Tests: none; body-only doc edit.
- Behavioural delta: future caco-profile agents inherit three real workflow learnings that this session paid for in practice.

## Operator-takeaway

A long, message-heavy session is a self-improvement asset, not fatigue: the durable learnings (verify against true GitHub not the lagging mirror; non-caco-dev profiles bypass the cargo gate so caco-profile Rust needs self-validation; anticipate doc-thrashing on fleet-wide operator announcements and HOLD+flag rather than race) are now in the profile itself. The biggest practical win of the session — catching and preventing a wrong ACA-cache doc revert via full-thread context — directly motivated gotcha #3.
