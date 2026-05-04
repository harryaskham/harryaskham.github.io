# Session summary — Clarify session-recording summary obligation

## Goal

Resolve the ambiguity in the session-recording mixin about whether plain
`direct` reintegration requires a summary, or only explicit `recorded` mode
does.

## Bead(s)

- `bd-86ddea` — [profile-audit] session summary requirement ambiguous for plain direct reintegration

## Before state

- Failing tests: none related
- The session-recording mixin said "every reintegration produces a record" and
  "do not reintegrate without one" but also said `recorded` is explicit-only,
  leaving plain `direct` agents unclear on whether summaries are mandatory.
- The caco-aks profile referenced "required recorded session summary" while
  defaulting to plain `direct`, compounding the confusion.

## After state

- Failing tests: none related
- session-recording.md now has an explicit **Summary obligation** block stating
  summaries are required whenever the mixin is composed, regardless of mode.
- Safety rails updated to match.
- caco-aks.md reworded to say summaries are required for both modes.

## Diff summary

- Commits: bc715eb55
- Files touched: `.cacophony/profiles/session-recording.md`, `.cacophony/profiles/caco-aks.md`
- Tests: no changes
- Behavioural delta: clarified that plain `direct` agents with session-recording
  composed must write summaries; explicit `recorded` additionally enforces
  section validation daemon-side.

## Operator-takeaway

The session-recording mixin now unambiguously requires summaries for all
reintegrating agents that compose it, not just those using the legacy
`recorded` mode. This was the root of the AKS agent's confusion about
whether to author summaries when using plain `direct`.
