# Session summary — caco-android-msd-1 (bd-6b1dea: pico send-failure banner immediacy fix)

## Bead
bd-6b1dea (bug, P2) — found during Harry's "test+improve recently landed" QA pass on my OWN bd-e35681 (pico StateFlow collect-site). A self-caught regression.

## Problem
picoSendFailure was assigned only in AgentDetailScreen.applyPicoUpdate() (per snapshot emission / 300ms poll). The onSendPrompt handlers (inline + fullscreen) called picoSource.sendPrompt(text) but did NOT update the banner — they relied on the loop. On my new FFI push path (bd-e35681), a failed send (sendPrompt sets lastSendFailure synchronously but often produces NO new snapshot) meant the send-failure banner could fail to appear / appear late. Regression vs the old 300ms poll.

## Fix
Re-read lastSendFailure immediately after sendPrompt in BOTH onSendPrompt handlers (sendPrompt sets it synchronously — FfiPicoSessionSource L138 / PicoSessionClient L158):
  onSendPrompt = { text -> if (picoSource.sendPrompt(text)) picoInput = ""; picoSendFailure = picoSource.lastSendFailure }
Banner is now immediate + correct on BOTH paths (push-safe + improves the OkHttp 300ms latency to instant).

## Discipline applied
Grepped ALL test files for sendPrompt/sendFailure pins: PicoAgentDetailSourceTest pins 'picoSource.sendPrompt(text)' (kept), others pin dialog signature/label (untouched) — no collateral breakage. Added source-pin sendPromptUpdatesSendFailureBannerImmediately_bd_6b1dea (>=2 handlers update picoSendFailure after sendPrompt). Validated FULL :app:testDebugUnitTest (not single --tests).

## Validation
FULL gradle :app:testDebugUnitTest — BUILD SUCCESSFUL, all green.

## Diff
Landed via reintegration receipt (see merge commit footer bd-6b1dea).
