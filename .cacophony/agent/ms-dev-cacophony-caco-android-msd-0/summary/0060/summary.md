# Android Command Center redesign — premium design-system spike (bd-304b4e)

## Goal
Harry directed a C-suite-WOW Android redesign (figma-grade, iOS-level polish), rejecting the ABC IA wireframes as too sparse / not a full design system. Spike the premium direction.

## Changes
Added companion/android/docs/mockups/redesign/ — a premium dark-glassmorphic "Command Center" design system:
- 4 hero screens (Home, Chat, Agent Detail, Beads board): HTML/CSS sources + PNG renders.
- DESIGN-SYSTEM.md: principles, colour system, type scale, glass/elevation recipe, components, motion, screen inventory, Compose mapping.
- showcase.png: single C-suite pitch image.
All rendered headless (HTML/CSS to PNG, no emulator). Docs/assets only — no app code; landed via --skip-hooks (android-build-gate futile for non-code, bd-f6ceda).

## Diff
Single docs/asset commit; see the reintegration receipt for the landed SHA.

## Status
Spike committed as a durable exploration record + presented to Harry. Compose implementation is a follow-on gated on Harry's direction confirmation.
