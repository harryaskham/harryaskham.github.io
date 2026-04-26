# Session summary — ms-mac daemon restart audit

## Goal

Audit the router-filed bd-58fae6 report that ms-mac had repeated daemon crashes after the 1.2.559 update, preserving evidence while avoiding destructive recovery actions.

## Bead(s)

- `bd-58fae6` — [ms-mac] Daemon crashed repeatedly after 1.2.559 update under load

## Before state

- Failing tests: none; this was an operational audit/docs slice.
- Relevant metrics: router report cited an 08:36Z restart during the 1.2.553 to 1.2.559 jump and a 09:14Z outage after roughly thirty-eight minutes of uptime.
- Context: related pressure was already tracked separately in `bd-69545e` for disk and `bd-0c272e` for caco-web/read-side flapping.

## After state

- Failing tests: none.
- Relevant metrics: live ms-mac status showed daemon, TTS, and caco-web running; daemon `started_at: 2026-04-26T09:14:41Z`; load remained high with about 4.1 GiB swap used and the Cacophony runtime around 115 GiB.
- Context: logs showed graceful restart markers, not a panic/backtrace: `daemon stopped: restart`, SIGTERM reason restart, and later `daemon started (v1.2.559)`.

## Diff summary

- Commits: `4650f6567`
- Files touched: `docs/audits/bd-58fae6-ms-mac-daemon-restart-under-load.md`
- Tests: `git diff --check`
- Behavioural delta: no runtime code changed. The audit records that the latest crash-looking reports are supervisor-managed restarts under load unless future evidence lacks graceful restart markers.

## Operator-takeaway

ms-mac is currently recovered, and the reported daemon “crashes” look like explicit restart windows under high local load rather than daemon panics; disk and caco-web pressure remain tracked on their own beads.
