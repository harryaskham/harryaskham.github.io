# Session summary — helsinki restart-window recurrence

## Goal

Re-audit reopened `bd-bafc96` after router observed a second short helsinki daemon and beads-primary outage, and determine whether the recurrence was an unexplained failure or another explicit lifecycle restart window.

## Bead(s)

- `bd-bafc96` — Helsinki daemon and beads primary briefly stopped during health pass

## Before state

- Failing tests: none; this was an operational audit/doc update.
- Relevant metrics: router report said helsinki daemon and beads primary were temporarily not running around `2026-04-26T03:18Z`, supervisor stayed active, and bounded rechecks recovered without router remediation.
- Context: the prior audit already showed an explicit `caco restart` window explaining the first report.

## After state

- Failing tests: none.
- Relevant metrics: live `caco status --json`, observer `caco bd status --json`, direct helsinki `caco status --json`, and direct helsinki `caco bd status --json` all showed helsinki serving again; `git diff --check` passed.
- Context: daemon and supervisor logs show the recurrence aligned with explicit `caco restart` windows, including `Apr 26 03:18:31 ... caco restart — node: helsinki, scope: all` and daemon `SIGTERM, reason=restart` markers.

## Diff summary

- Commits: `12f8e1835`
- Files touched: `docs/audits/bd-bafc96-daemon-restart-window.md`
- Tests: no code tests; audit validation was live status/log checks plus markdown diff whitespace check.
- Behavioural delta: documentation now records the recurrence and clarifies that repeated health-pass outages are restart windows on the authority node, not beads-primary split-brain.

## Operator-takeaway

The second bd-bafc96 report was also explained by first-party restart activity on helsinki. Because helsinki is the sole beads-primary candidate, short daemon restarts still look like daemon plus beads-primary outages until the listener returns.
