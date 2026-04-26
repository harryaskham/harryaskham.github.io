# Session summary — caco-web snapshot freshness hero alignment

## Goal

Fix an operator-trust inconsistency found by the proactive caco-web Playwright duty cycle: the status page top freshness indicator warned that snapshot data was partial/stale while the prominent hero quick-facts pill still called the snapshot fresh.

## Bead(s)

- `bd-f7a201` — Show partial snapshot state in caco-web hero freshness pill

## Before state

- Failing tests: none known.
- Relevant metrics: Playwright snapshot against managed caco-web on port 11180 showed top freshness text `beads: partial, agents: stale` alongside hero text `Fresh snapshot · 13s ago`; console had 0 errors and network requests returned 200 for the observed route.
- Context: `renderStatusHero()` only used `lastSnapshotTime` and SSE age for the hero freshness pill, while `applySnapshot()` used the daemon `snap.freshness` domain statuses for the top indicator.

## After state

- Failing tests: none known.
- Relevant metrics: patched browser repro on port 49333 showed top freshness text `beads: partial, agents: stale` and hero text `Snapshot degraded · 12s ago` with tooltip naming `beads: partial, agents: stale`; console had 0 errors and observed network requests returned 200.
- Context: `applySnapshot()` now stores the latest snapshot freshness payload, and `renderStatusHero()` uses it to either show the live-SSE override when SSE is fresh or surface a degraded snapshot state when domains are partial/stale.

## Diff summary

- Commits: `10bd1dbca`
- Files touched: `crates/caco-web/static/app.js`, `crates/caco-web/src/tests.rs`
- Tests: +1 / -0 / flipped 0
- Behavioural delta: The status hero no longer labels a recent but partial/stale snapshot as fresh; the prominent quick-facts pill now agrees with the top freshness warning and names degraded domains in its tooltip.

## Embedded artefacts

- `screenshots/before-freshness-mismatch.png` — managed caco-web before screenshot showing the hero freshness mismatch.
- `screenshots/after-degraded-freshness.png` — patched caco-web after screenshot showing `Snapshot degraded` in the hero pill.

## Operator-takeaway

The first proactive caco-web duty-cycle pass found and fixed a real dashboard trust issue: the UI now avoids giving a false “fresh” signal when the daemon says bead or agent snapshot domains are degraded.
