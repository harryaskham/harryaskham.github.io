# Session summary — live AURORA Home/cluster-pulse hero (md2-1)

## Goal
Build the second live in-app AURORA hero — the signature Home/cluster-pulse moment (fleet as a living constellation, not a list) — as a real compiling Compose screen on the shared AuroraTokens, pairing with md2-0's landed live Pico so Harry sees in-app polish on two screens.

## What landed
- `companion/android/app/src/main/java/com/cacophony/companion/ui/home/AuroraHomeHero.kt` — live Compose realization of the Home hero against the canonical AuroraTokens (md2-0's landed token layer, 049ae1716): a Canvas-drawn cluster-pulse constellation (radial aurora-bloom pulse core + orbiting status nodes in live/pending/blocked colors + faint orbit rings + connectors), a focus-elevation needs-you glass card, a 3-stat glass row (agents/ready/in-flight with Display numbers), an aurora-orb pico FAB, and the glass Home/Talk/Agents/Work/More nav.

## Design fidelity
- Uses AuroraTokens 1:1 (auroraGlass elevations, AuroraOrbBrush/radial bloom, AuroraStatus colors, Display/Title/Body/Caption type, AuroraVoid canvas) — guaranteed consistent with md2-0's live Pico since both consume the same source-of-truth tokens.
- Conservative Compose only (Box/Column/Row/Text/Spacer/Canvas + token modifiers), @Preview included — mirrors md2-0's Pico pattern so it compiles without material-icons-extended/TextField coupling.

## Scope / follow-ups
- Presentation-faithful: the constellation nodes + stat counts are representative; the live bind to the real fleet snapshot (agent/bead/reintegration counts) + the orbit animation are follow-ups. The cluster-pulse Canvas is the live-bind seam (mirrors Pico's static-then-live approach).

## Validation
- Queued android compile tj-a8b7d1bf (.#android-validation :app:compileDebugKotlin).

## Operator-takeaway
- Two live tappable AURORA heroes now exist for Harry: Pico-chat (md2-0, landed) + Home/cluster-pulse (this) — real in-app polish, not static renders, both on one shared design system. On Harry's confirm: expand the full live set, flag caco-ios-1 (iOS parity), file Phase-1 adoption beads. Android-only Kotlin — echo gate, no cargo gate.
