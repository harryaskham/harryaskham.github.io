# AURORA Work/Beads board LIVE Compose hero (bd-304b4e)

## Goal
Build the 4th canonical AURORA live hero — the Work/Beads board — completing live Compose coverage alongside Home/cluster-pulse + Pico-chat. Clean split with md2-1 (I own Work/Beads live, md2-1 owns Agent-detail bd-1336c3).

## Changes
- NEW companion/android/app/src/main/java/com/cacophony/companion/ui/home/AuroraBeadsHero.kt: live Compose Work/Beads hero on AuroraTokens.kt — a throughput-pulse Canvas (weekly aurora-gradient bars), status filter pills (Ready/In-progress/Blocked), status-accented glass bead cards (leading status rail + mono bead id + status chip + priority badge + label chips), aurora-orb new-bead FAB, Work nav. Conservative Compose (Box/Column/Row/Text/Spacer/Canvas + AuroraTokens modifiers), @Preview. Pattern matches AuroraHomeHero.kt.
- AuroraPreviewScreen.kt: added a Work toggle tab surfacing AuroraBeadsHero (now 3 live heroes: Home/Pico/Work).
- ux-revamp-2026.md sec17: registered the Work/Beads live hero (3 live Compose heroes; crew split noted).

## Validation
Queued android-validation gradle :app:compileDebugKotlin (bj-f4f01b43) — SUCCEEDED.

## Diff
See reintegration receipt for the landed SHA.
