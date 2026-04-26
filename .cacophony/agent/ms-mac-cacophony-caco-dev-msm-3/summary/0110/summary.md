# Session summary — restart convergence hardening after stale-cache rollout

## Goal

This session responded to the reopened `bd-2efa9c` health evidence after the peer-snapshot cache fix landed. The immediate stale/list/direct symptom cleared on router recheck, but the attempt to deploy the just-built daemon on ms-mac exposed a restart-convergence footgun: a healthy daemon build could be rolled back or destabilized because startup sync took longer than the lifecycle window and optional web health participated in deep promotion checks.

## Bead(s)

- `bd-2efa9c` — Active workers remain marked stale with impossible no-tool-activity age
- Follow-up filed: `bd-38daf0` — `[ms-mac] 1.2.561 daemon stack overflow during caco-web restart window`

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: router pass at 13:15Z saw `cqqcqwtn68gktqrr` stale by summary/direct while `agent list --state stale` was empty; later pass at 13:33Z cleared stale state again.
- Context: ms-mac was running stable 1.2.559 while the source tree contained the landed 1.2.561 peer-snapshot cache fix. A scoped `caco-daemon` restart with the built checkout binary initially rolled back while full-state sync was still progressing, and a later promotion hit a daemon stack overflow near caco-web restart/watchdog activity.

## After state

- Failing tests: none in focused validation; existing `microvm.rs` warnings remain unrelated during caco-cli checks.
- Relevant metrics: validation passed for the new optional-service deep-probe test, the existing permanent-failure deep-probe test, `cargo check -p caco-sidecar`, and `cargo check -p caco-cli`.
- Context: lifecycle health uses a 90s startup window for busy multi-node startup phases, and deep HTTP health probes now skip all non-critical services rather than only non-critical pid-only services. ms-mac was restored to the previous known-good 1.2.559 launcher after the stack-overflow attempt; no further live restart was attempted in this slice.

## Diff summary

- Commits: `ea0496290`, `781f4c9d4`
- Files touched: `crates/caco-cli/src/lib.rs`, `crates/caco-sidecar/src/lifecycle.rs`, `README.md`, `AGENTS.md`, `docs/daemon.html`
- Tests: added `deep_health_probe_skips_noncritical_http_services`; updated deep health probe filtering for existing tests.
- Behavioural delta: `caco up` / `caco restart` wait longer for daemon startup phases such as full-state sync before launcher rollback, and optional HTTP services such as caco-web no longer gate daemon launcher promotion. Optional service health is still reported separately.

## Operator-takeaway

The stale-worker recurrence itself cleared after the earlier cache fix, but deployment showed the restart path could still turn an otherwise healthy daemon rollout into noise or rollback because optional web health and short startup windows were treated as promotion blockers. This slice hardens that lifecycle path and files the separate stack-overflow crash as `bd-38daf0` rather than burying it under stale-liveness work.
