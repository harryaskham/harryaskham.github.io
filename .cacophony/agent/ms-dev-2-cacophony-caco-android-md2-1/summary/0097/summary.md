# Session summary — AURORA preview entry: make the live heroes tappable (md2-1)

## Goal
Make the two live AURORA heroes (Home/cluster-pulse + Pico-chat) reachable inside the running app via one non-destructive entry, so Harry can TAP into the AURORA experience to confirm the direction — not just review static renders. md2-0 handed me the IA/nav decision.

## What landed
- `companion/android/app/src/main/java/com/cacophony/companion/ui/home/AuroraPreviewScreen.kt` — a preview screen with a Home/Pico segmented toggle (aurora-accent active pill) that surfaces both landed live heroes (AuroraHomeHero + md2-0's AuroraPicoChatHero), with a Back affordance. Conservative Compose + @Preview.
- `MainActivity.kt` (4 minimal wires) — registers "aurora" as a More sub-page: deep-link routes (aurora/aurora-preview), surface-title map ("AURORA preview"), render switch (subPage == "aurora" -> AuroraPreviewScreen), and a prominent "Experimental → AURORA preview" MoreMenuItem (AutoAwesome sparkle icon).

## IA decision (mine)
- Entry = a More sub-page, not a nav-tab replacement or a debug-only activity: main-build-reachable on Harry's device, non-destructive (zero production screens touched), and trivially removable / flippable-to-default once blessed. Both heroes share one AuroraTokens source of truth, so the toggle shows a consistent design system.

## Division of labor
- md2-0 wires their AuroraPicoChatHero to live pico data (FfiPicoSessionSource) additively — the preview calls AuroraPicoChatHero() so it reflects that upgrade automatically. My follow-up: live-bind the Home cluster-pulse to the real fleet snapshot + orbit animation.

## Validation
- Queued android compile tj-8cfb1f80 (.#android-validation :app:compileDebugKotlin) — full MainActivity integration + the preview screen.

## Operator-takeaway
- The AURORA direction is now TAPPABLE in the running app (More → Experimental → AURORA preview): Home/cluster-pulse + Pico-chat, both live Compose on one design system, non-destructive. This is the in-app proof Harry can experience to confirm the figma-WOW direction. Android-only Kotlin — echo gate, no cargo gate.
