# Session summary — Agent groups and group-scoped chat contract

## Goal

Define a canonical product contract for agent groups and group-scoped chat so parallel web, TUI, mobile, desktop, config, and chat implementation slices can converge on the same semantics without stepping on each other.

## Bead(s)

- `bd-3ee7bf` — Define canonical agent groups and group-scoped chat contract

## Before state

- `SPEC.md` documented `agents.groups` only as an optional config map of group names to member IDs/declaration names.
- There was no normative cross-surface contract for what an agent group means, how unresolved members appear, what group-scoped chat targets do, or how TUI/web/Android/iOS/macOS should present groups consistently.
- Adjacent implementation work was active in parallel: winmini config schema semantics, po4-2 TUI group display, and Aurora group-scoped chat functionality.

## After state

- Added `SPEC.md` section `6.5.0 Agent Groups and Group-Scoped Chat`.
- Defined agent groups as read-only operator-facing cluster-scoped views over concrete agents, not scheduler pools, authorization scopes, separate chat rooms, or permission grants.
- Specified daemon/API read-model requirements including `agent_groups`-style collection, ordered `members`, `unresolved_members`, member provenance, status/freshness, and derived counts.
- Defined group-scoped chat as canonical per-agent message fanout with `group_id` provenance and bounded per-target delivery diagnostics.
- Added cross-surface presentation requirements for TUI, caco-web, Android, iOS, and macOS.

## Diff summary

- Code/content commits: `039e45430` (final landed squash SHA will come from the reintegration receipt).
- Files touched: `SPEC.md`.
- Tests: source/docs-only validation: `git diff --check` passed.
- Behavioural delta: no runtime code changed; this is a normative contract for ongoing implementation slices.

## Embedded artefacts

None.

## Operator-takeaway

Agent groups now have a single contract: they are config-defined, read-only operator views that resolve to concrete agents with provenance and unresolved-member visibility; group chat is an addressing/fanout mode over canonical messages, not a separate room. This gives the active config, TUI, web/chat, and companion-app workers a shared target while avoiding schema or implementation overlap in this slice.
