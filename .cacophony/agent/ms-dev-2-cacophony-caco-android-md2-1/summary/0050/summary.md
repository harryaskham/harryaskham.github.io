# Session summary — bd-207574 pending-dialog cache contract (Android widget reply-affordance render data)

## Goal
Land the pending-dialog cache contract the widget reply affordance needs (bd-207574, md2-1 lane contribution) — so md2-0's snapshot hook can write the pending-dialog state and the widget render can pick the right affordance.

## Bead(s)
- bd-207574 (md2-0's bead; this is the md2-1 cache-contract contribution). md2-0 folds the hook-write + the confirm/cancel BroadcastReceiver + transient daemon POST; md2-1 renders the affordance.

## Before state
AgentWidgetData carried status/tmux/pico but no pending-dialog state, so a widget could not know whether (or how) to show a reply affordance.

## After state
- AgentWidgetData: + pendingDialogId, pendingDialogMethod, pendingDialogPrompt.
- AgentWidgetDataStore.savePicoPendingDialog(ctx, agentId, id, method, prompt): writes ONLY those 3 keys, leaving picoTranscript/the other fields/updatedAt untouched; a blank id clears them (dialog resolved). Folded into load() + delete().
- Contract: md2-0's snapshot-observation hook (extends bd-2b6f90) calls savePicoPendingDialog when snapshot.pendingDialog (PicoExtensionUiRequest) is blocking — id/method/prompt-from-rawJson — and clears it (blank id) when resolved. The widget render reads pendingDialogMethod: confirm/cancel -> direct buttons (wired to md2-0's receiver), value/input/editor -> tap-to-app intent, blank -> no affordance.

## Diff summary
Landed squash-merged on main — see the reintegration receipt for the final SHA. Edits: AgentWidgetDataStore.kt (3 fields + savePicoPendingDialog + load/delete); new AgentWidgetPendingDialogSourceTest.kt.

## Embedded artefacts
- Full :app:testDebugUnitTest: 1862 tests, 0 failures+errors (no regression).
- AgentWidgetPendingDialogSourceTest 3/0 (fields, save/clear, load surfacing).
- :app:assembleDebug success.

## Operator-takeaway
The contract-first cache piece for bd-207574's reply-from-widget. Same proven pattern as the slice-1a savePicoTranscript hook: md2-1 lands the cache contract (the field shape + write/clear), md2-0 builds the hook-write + the out-of-process confirm/cancel send-path, md2-1 renders the affordance once md2-0 pings the receiver action string. Remaining on bd-207574: md2-0's BroadcastReceiver + transient POST; md2-1's confirm/cancel buttons + text tap-to-app render.
