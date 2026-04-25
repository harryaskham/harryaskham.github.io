# Session summary — helsinki daemon inbox transient audit

## Goal

Audit the two reported `caco msg inbox` / daemon-reachability transients on helsinki and determine whether they were unexplained daemon stalls or explainable lifecycle windows.

## Bead(s)

- `bd-7cd0fc` — helsinki daemon: intermittent connection-refused on `/api/v1/projects/cacophony/messages/inbox`

## Before state

- Failing tests: none; this was an operational log-audit bead.
- Relevant metrics: two reported transient failures, one around 21:00 BST and one around 22:50 BST on 2026-04-25. Symptoms recovered without restart intervention.
- Context: the bead asked for log/metric audit on helsinki and escalation to P2 only if the pattern looked recurrent outside expected restart behavior.

## After state

- Failing tests: none observed.
- Relevant metrics: helsinki daemon logs showed restart windows at `20:47:00Z -> 20:48:12Z` and `21:47:03Z -> 21:48:13Z`, plus post-restart startup reconciliation churn. A live `caco node status --node helsinki --json` showed `caco-daemon` running on version `1.2.552` after the audit.
- Context: the audit concluded the reported connection-refused windows align with daemon restart/grace windows rather than a standalone accept-loop stall or DB lock.

## Diff summary

- Commits: `b52edc220`
- Files touched: `docs/audits/bd-7cd0fc-helsinki-daemon-inbox-transient.md`
- Tests: no code tests; validation was `git diff --check` plus direct helsinki log/status inspection.
- Behavioural delta: no runtime behavior changed; the repo now has a durable audit note with evidence, conclusion, and escalation triggers.

## Operator-takeaway

No P2 escalation is warranted from the current evidence: both observed helsinki inbox failures line up with daemon restart windows. Escalate only if connection-refused recurs without nearby `daemon stopped` / `daemon started` log pairs, or if endpoints stay unavailable while status reports the daemon running outside restart grace.
