# Session summary — continued macOS Tendril QA artefacts

## Goal

Keep the operator-requested native macOS app QA loop moving while ms-mac TTS health was being stabilized, using low-resolution Tendril capture/action/verify passes and filing focused UX beads for issues found.

## Bead(s)

- `bd-4defb0` — [macOS visual QA] Continue full-surface Tendril polish loop
- `bd-d87c61` — [ms-mac TTS routing fix] local-device runtime routing recovery support
- `bd-a606d7` — ms-mac TTS feed-watcher / control-port recovery support

## Before state

- Failing tests: unrelated broken-on-main clippy issues were owned by other agents and not duplicated.
- Relevant metrics: ms-mac TTS had been unstable during routing changes; installed Cacophony.app remained visually stuck on Beads for many interactions.
- Context: The operator repeatedly asked to keep work moving, keep ms-mac healthy, and ensure TTS playback while also continuing the Tendril app loop.

## After state

- Failing tests: none introduced; this chunk is artefact and operational evidence only.
- Relevant metrics: TTS route is `local-device`; recent trace entries show `sink=local-device` and `terminal outcome=played`; OS output reports MacBook Pro Speakers selected. Captures through summary 0027 are committed.
- Context: Filed/confirmed UX issues for sidebar hit targets, stale installed-app provenance, Beads search input, and Beads row detail selection. Later captures reinforce those issues without duplicating beads.

## Diff summary

- Commits: `8c3e898dd`, `3fb08f9a1`, `dc551c71a`
- Files touched: `.cacophony/agent/ms-mac-cacophony-caco-dev-msm-1/summary/0014..0027/screenshots/*.png`
- Tests: +0 / -0 / flipped 0
- Behavioural delta: No product code changed; this lands durable visual QA evidence and TTS operational context for operator review.

## Embedded artefacts

- `screenshots/*.png` — Low-resolution Tendril captures showing installed app interactions staying on Beads, search/filter no-op behavior, row detail selection no-op, and project-picker no-op behavior.

## Operator-takeaway

The current installed macOS app is still useful for visual QA but appears stale or interaction-blocked: many semantic interactions leave it on Beads. TTS has been routed to local-device and trace-verified, so the QA loop can continue without further destructive service work.
