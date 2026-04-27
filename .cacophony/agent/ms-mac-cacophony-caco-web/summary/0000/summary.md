# Session summary — caco-web recorded artifact profile update

## Goal

Update the persistent caco-web operating profile so future duty cycles preserve Playwright and validation artifacts under the recorded-summary tree, commit those artifacts with the cycle summary, and reintegrate through the direct recorded lifecycle path instead of leaving evidence only in transient `/tmp` or `.playwright-cli` locations.

## Bead(s)

- `bd-09b32d` — caco-web profile should persist duty-cycle artifacts in recorded summaries

## Before state

- Failing tests: none known for this profile-only change.
- Relevant metrics: caco-web profile default reintegration mode was `pr_review`; duty-cycle instructions allowed artifact paths in `/tmp` / `.playwright-cli` chat reports and did not require observation-only cycles to commit recorded summaries.
- Context: Harry asked directly for the caco-web profile to put all artifacts in the summaries directory and commit plus reintegrate via `direct,recorded`.

## After state

- Failing tests: none known.
- Relevant metrics: caco-web profile frontmatter now sets `reintegration.mode: direct,recorded` and `allowed_modes: [direct,recorded]`; Active Duty Cycle, Playwright setup, artifact handling, reporting, and reintegration sections now require recorded-summary artifacts and `direct,recorded` reintegration for every duty cycle.
- Context: Future no-bead observation cycles must write `summary.md`, keep bounded Playwright artifacts under `.cacophony/agent/$CACO_AGENT_ID/summary/<index>/web/`, commit the summary/artifacts, and reintegrate before reporting completion.

## Diff summary

- Commits: this recorded-summary commit (`bd-09b32d: require caco-web recorded artifacts`; final SHA is the reintegration tip because the summary is committed with the change)
- Files touched: `.cacophony/profiles/caco-web.md`, `.cacophony/agent/ms-mac-cacophony-caco-web/summary/0000/summary.md`, `.cacophony/agent/ms-mac-cacophony-caco-web/summary/0000/web/profile-frontmatter-validation.log`, `.cacophony/agent/ms-mac-cacophony-caco-web/summary/0000/web/profile-show-daemon-before-sync.log`.
- Tests: profile frontmatter parsed with PyYAML and asserted `direct,recorded`; `caco profile show --name caco-web` captured the daemon's pre-sync profile state for comparison.
- Behavioural delta: caco-web is now instructed to treat recorded summaries as the durable artifact root for both implementation and observation-only cycles, and to use direct recorded reintegration as the normal lifecycle path.

## Embedded artefacts

- `web/profile-frontmatter-validation.log` — local parse/validation proof for the edited profile frontmatter.
- `web/profile-show-daemon-before-sync.log` — daemon profile snapshot before this profile update has been reintegrated and synced.

## Operator-takeaway

Future caco-web cycles should no longer finish with only transient Playwright evidence and a chat report: the profile now requires durable summary-directory artifacts plus a committed `direct,recorded` reintegration for each cycle.
