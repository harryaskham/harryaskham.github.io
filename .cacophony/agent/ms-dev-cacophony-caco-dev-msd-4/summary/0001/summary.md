# Session summary — helsinki restart-window audit

## Goal

Determine whether the P1 report of helsinki `caco-daemon` and beads-primary briefly stopping during a health pass represented an unexplained authority failure or a known lifecycle restart window, and leave an operator-readable audit trail.

## Bead(s)

- `bd-bafc96` — Helsinki daemon and beads primary briefly stopped during health pass

## Before state

- Failing tests: none; this was an operational audit.
- Relevant metrics: router health saw daemon/beads-primary unavailable around `2026-04-26T02:27Z`, then recovered without remediation.
- Context: helsinki is the authoritative beads host, so a daemon restart can temporarily make both daemon and beads API checks look like an authority outage.

## After state

- Failing tests: none.
- Relevant metrics: live checks showed helsinki reachable again, `beads_host.reachable: true`, `caco bd status --json` successful, and daemon `started_at: 2026-04-26T02:27:46Z`.
- Context: daemon and supervisor logs show explicit `caco restart` / graceful SIGTERM restart markers, not a panic or split-brain.

## Diff summary

- Commits: `948b14936`
- Files touched: `docs/audits/bd-bafc96-helsinki-daemon-restart-window.md`
- Tests: `git diff --check` passed for the audit document.
- Behavioural delta: no code changed; the incident is documented as a restart-window explanation with follow-up triggers and capture commands.

## Operator-takeaway

The helsinki beads-primary outage was an explicit `caco restart` window: the authority recovered normally, and future escalation should focus on missing restart markers or restart windows long enough to need clearer `maintenance_window` surfacing.
