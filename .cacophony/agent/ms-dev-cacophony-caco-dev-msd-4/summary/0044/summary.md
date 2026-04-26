# Session summary — helsinki restart recurrence audit

## Goal

Reconcile the reopened bd-bafc96 health-pass report by checking whether the latest helsinki daemon/beads-primary outage was a new failure mode or another supervisor-managed restart window.

## Bead(s)

- `bd-bafc96` — Helsinki daemon and beads primary briefly stopped during health pass
- Follow-up filed: `bd-f70ce1` — Surface beads-primary restart windows as maintenance state

## Before state

- Failing tests: none; this was an operational audit/docs update.
- Relevant metrics: router reported helsinki daemon and beads primary both down during a health pass, then recovered on bounded recheck.
- Context: prior bd-bafc96 audit already showed the same symptom during explicit `caco restart` windows, but the bead was reopened after recurrence.

## After state

- Failing tests: none.
- Relevant metrics: direct helsinki checks showed daemon reachable, beads host reachable and authoritative, `active_primary: helsinki`, and daemon `started_at: 2026-04-26T09:23:07Z` after recovery.
- Context: daemon and service logs showed explicit restart markers around 09:22–09:23Z, including `daemon stopped: restart`, SIGTERM reason restart, and service supervisor `caco restart — node: helsinki, scope: all`.

## Diff summary

- Commits: `44ffe5530`
- Files touched: `docs/audits/bd-bafc96-daemon-restart-window.md`
- Tests: `git diff --check`
- Behavioural delta: no runtime code changed. The audit now captures the latest recurrence and files a follow-up for a proper beads-primary restart-window maintenance signal.

## Operator-takeaway

The latest helsinki daemon/beads-primary outage-looking snapshot was again an explicit restart window, not split-brain or a crash; the product gap is that health summaries need a maintenance/restart-window signal so this does not keep reopening as an unexplained P1.
