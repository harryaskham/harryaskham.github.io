# Session summary — blocked auto-claim handoff clarification

## Goal

Clarify the worker and auto-claim contract for beads that become dependency-blocked while already claimed. The immediate goal was to prevent the confusion seen during bd-c0bb58, where a blocked bead later became ready and was auto-claimed by another worker despite preserved implementation/handoff context.

## Bead(s)

- `bd-a02866` — Clarify auto-claim behavior when a bead is dependency-blocked mid-claim

## Before state

- Failing tests: none observed; this is documentation/profile guidance.
- Relevant metrics: no code-path metrics; current guidance described dependency readiness and auto-claim ownership verification but did not spell out blocked-mid-work reservation semantics.
- Context: workers could infer that adding a dependency to an in-progress bead implicitly reserved it for the prior owner after the blocker closed, which is not how the ready queue behaves.

## After state

- Failing tests: none in documentation/config validation.
- Relevant metrics: `git diff --check` passed; `caco config validate` passed.
- Context: auto-claim guidance now says blocked/unclaimed beads may be claimed by any idle worker once ready, and that previous owners must either stay claimed while actively waiting or preserve work plus communicate an explicit handoff/unclaim.

## Diff summary

- Commits: `e2737c42f` (guidance/docs); recorded summary in this commit
- Files touched: `.cacophony/profiles/auto-claim.md`, `AGENTS.md`, `README.md`, `SPEC.md`
- Tests: +0 / -0 / flipped 0; validation was docs/config focused.
- Behavioural delta: no runtime behaviour changes; worker instructions and normative claim semantics now make the ownership/handoff rule explicit.

## Operator-takeaway

Dependency updates are not implicit reservations. If a worker blocks a bead mid-work, it must make the next owner obvious by either staying claimed or preserving work with a clear handoff before unclaiming, and later auto-claimants must re-hydrate and honor that evidence before editing.
