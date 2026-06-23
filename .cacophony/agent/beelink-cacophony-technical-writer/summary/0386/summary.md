# Technical-writer review summary

## Goal

Document the now-complete forge-true reintegration verification (resolves the
mirror-lag false-failure class), which was deferred until the forge-verify family
landed.

## Bead(s)

- `bd-c63005` — technical-writer documentation maintenance.
- Documents bd-0ec380 (parts 1+2), bd-89088a, bd-d4b93b (mirror-lag false-failure).

## Before state

- `docs/reintegration-policy.md`/`.html` said direct reintegration verifies the commit is "reachable from the configured remote" without noting that, when origin is a daemon-local canonical mirror, the verify previously trusted the possibly-lagging mirror (the bd-d4b93b false-failure class). I had deferred this until the forge-verify family (bd-0ec380 part 2) completed.

## After state

- Both now note: when the checkout's `origin` is a daemon-local canonical mirror rather than the forge, the publish compare-and-swap and post-publish reachability check `ls-remote` the mirror's own `origin` (the true forge / GitHub tip) instead of the lagging mirror, so a reintegration whose code actually landed on true `main` is not falsely reported as failed by a stale mirror.
- Verified against `crates/caco-daemon/src/reintegration.rs` (forge-tip `ls-remote` comments, bd-0ec380/bd-89088a). Sibling marker refreshed; `validate-pages.sh` passed.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `docs/reintegration-policy.md`, `docs/reintegration-policy.html`.
- Behavioural delta: documentation only.

## Operator-takeaway

The reintegration-policy docs now explain why a previously mirror-lag-false-failed
reintegration verifies correctly, closing a long-standing gated docs item.
