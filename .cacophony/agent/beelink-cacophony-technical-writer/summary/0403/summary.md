# Technical-writer review summary

## Goal

Extend the reintegration-policy forge-verify note: the caco-cli post-land
re-verify now also routes its ancestry check through the true forge tip
(bd-3c9195) with transient settle/retry (bd-c6d7e0).

## Bead(s)

- `bd-3c9195` — caco-cli landed-commit re-verify through true forge tip (permanent mirror-lag bd-d4b93b robustness).
- `bd-c6d7e0` — retry/settle for transient PR-squash-merge false main-integrity drops.
- `bd-c63005` — technical-writer documentation maintenance.

## Before state

- reintegration-policy.md/.html documented the DAEMON-side forge-true-verify (publish compare-and-swap + post-publish reachability ls-remote the true forge, bd-0ec380/bd-89088a/bd-d4b93b) but not the CLI-side post-land re-verify, which bd-3c9195 just routed through the same true forge tip.

## After state

- The step-7 forge-verify note now adds: the CLI-side post-land re-verify (`reverify_recent_landed_commits`) routes its ancestry check through the same true forge tip rather than the daemon-local mirror, for permanent-mirror-lag robustness (bd-3c9195), with bounded settle/retry to absorb transient post-PR-squash-merge false main-integrity drops (bd-c6d7e0).
- Verified against the bd-3c9195/bd-c6d7e0 commit bodies (reverify_recent_landed_commits_best_effort fetched the daemon-local mirror; now routes to the forge). Sibling marker refreshed; validate-pages passed.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `docs/reintegration-policy.md`, `docs/reintegration-policy.html`.
- Behavioural delta: documentation only.

## Operator-takeaway

The forge-verify docs now cover both halves of mirror-lag robustness: the daemon
publish-verify and the caco-cli post-land re-verify both route through the true
forge tip, so neither falsely reports a landed reintegration as reverted.
