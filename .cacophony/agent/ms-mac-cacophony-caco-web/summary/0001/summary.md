# Session summary — proactive caco-web profile duties

## Goal

Encode Harry's correction that the persistent caco-web worker must actively improve the browser dashboard: proactively scan caco-web/web-dashboard beads, run lightweight `playwright-cli` observation when no bead is active, and file or claim focused follow-up work from evidence instead of merely checking inbox.

## Bead(s)

- `bd-713867` — Strengthen caco-web proactive observation responsibilities

## Before state

- Failing tests: none known for this profile-only change.
- Relevant metrics: `caco profile show --name caco-web --json` parsed before editing; current profile already mentioned Playwright and an endless observation loop, but the live prompt loop and wording still allowed inbox-only behaviour.
- Context: the hard policy emphasized no autoclaim but did not clearly distinguish random queue draining from this agent's responsibility to claim unowned, focused caco-web/browser-dashboard beads.

## After state

- Failing tests: none known.
- Relevant metrics: `caco profile show --name caco-web --json` passed; `git diff --check -- .cacophony/profiles/caco-web.md` passed; in-session Pi loop was replaced with a 10-minute active-duty prompt covering inbox, web bead scan, Playwright observation, and evidence-backed filing/claiming.
- Context: the profile now states that quiet inboxes mean "drive the dashboard and look for the next improvement", adds an Active Duty Cycle, and clarifies that claiming unowned focused caco-web/browser-dashboard beads is expected while arbitrary backlog draining remains forbidden.

## Diff summary

- Commits: `27f258d07`
- Files touched: `.cacophony/profiles/caco-web.md`
- Tests: +0 / -0 / flipped 0
- Behavioural delta: Future caco-web persistent sessions should proactively run lightweight Playwright dashboard observation and claim focused web-dashboard work, instead of idling after an inbox check.

## Operator-takeaway

The caco-web profile now makes the proactive improvement loop explicit: this worker owns the browser dashboard surface and should continuously inspect it with `playwright-cli`, turn evidence into focused beads, and fix one safe web slice at a time.
