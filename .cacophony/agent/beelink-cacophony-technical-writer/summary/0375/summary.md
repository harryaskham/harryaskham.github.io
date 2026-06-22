# Technical-writer summary — refresh docs/wearable.html companion overview

## Goal

Begin the docs/wearable.html ("Phone & Watch Companion") refresh — the canonical
companion-app docs page was stale, mentioning none of the major recent surfaces. This
slice updates the most-read "What the companion surfaces do now" overview cards
(iPhone, Apple Watch, Android phone, Wear OS) to reflect the 06-19/06-20/06-21 work,
authored from verified bead titles.

## Bead(s)

- No implementation bead — routine technical-writer documentation freshness
  (companion-surface docs catch-up).

## Before state

- docs/wearable.html (HTML-only, 49139 bytes) overview cards described only the older
  surfaces; grep for Live Activity / Siri / App Intents / SSH tunnel / on-device
  inference / group chat / share extension / Smart Stack / embedded daemon = 0.

## After state

- The four overview cards now name the major recent additions: iPhone (Live Activity,
  Apple Assistant/Siri App Intents, self-managed SSH tunnels + SSH key management,
  on-device + vision inference via the daemon LLM API, group chat, image upload for
  VLM/bead attachments); Apple Watch (Live Activity Smart Stack — choices, chat
  streams, DM quick-reply — pico chat input, Assistant/Siri surface); Android
  (group chat, multi-port SSH tunnels for terminal + caco-web, in-process embedded
  daemon); Wear OS (SSH port-forward, full-screen live agent terminal with
  keyboard/dictation, assistant surface).
- 49960 bytes, under the 51200 (50 KiB) validate-pages budget. `./docs/validate-pages.sh`
  → PASS.

## Diff summary

- Files touched: docs/wearable.html (4 overview card paragraphs; HTML-only page, no
  .md sibling).
- Tests: n/a (docs-only); validation via docs/validate-pages.sh (PASS).
- Behavioural delta: documentation only.

## Operator-takeaway

The companion overview is current again. Follow-up: a deeper surface-by-surface
refresh of the Phone App / Watch App "Features" sections (and the macOS share
extension) would need a deliberate validate-pages budget bump for wearable.html
(it's a dense reference page near the 50 KiB default, like cli.html); the overview
slice fits within the default budget.
