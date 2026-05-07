# Session summary — summary SHA self-reference guidance

## Goal

Clarify the session-summary instructions so agents do not create a self-reference loop by listing the summary artefact commit's own mutable SHA in the summary they are still authoring.

## Bead(s)

- `bd-918fe3` — Clarify session summaries should not self-reference mutable summary commit SHA

## Before state

- Failing tests: none; this was a documentation/profile clarification slice.
- Relevant metrics: the `session-recording` template said `Commits: <sha1>, <sha2>` without distinguishing code/content commits from the summary artefact commit.
- Context: That wording could encourage agents to list a summary commit SHA inside the summary, even though amending the summary changes that SHA.

## After state

- Failing tests: none observed.
- Relevant metrics: lightweight validation passed with `rg` checks for the new guidance and `git diff --check`.
- Context: The profile template and repository docs now tell agents to cite known code/content commits or defer to the reintegration receipt, while omitting the summary artefact commit's own SHA.

## Diff summary

- Code/content commits: `fe02d2e5a`
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched: `.cacophony/profiles/session-recording.md`, `README.md`, `AGENTS.md`, `SPEC.md`
- Tests: +0 / -0 / flipped 0; documentation-only validation via `rg` and `git diff --check`
- Behavioural delta: Agent-facing summary guidance now avoids mutable summary-SHA self-reference loops.

## Operator-takeaway

Future session summaries should point at stable code/content evidence or the reintegration receipt, not at their own summary commit, so agents can amend summaries without chasing a moving SHA.
