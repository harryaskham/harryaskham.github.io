# Session summary — bd-207574 direct confirm/cancel buttons (completes reply-from-widget)

## Goal
Wire the widget's direct Confirm/Cancel buttons to md2-0's out-of-process PicoDialogReplyReceiver — the final piece of bd-207574 (reply directly from the agent widget without opening the app).

## Bead(s)
- bd-207574 (md2-0's bead) — COMPLETED by this button-wiring (md2-0: "your button-wiring closes bd-207574"). Also fixes a broken-on-main test (AgentWidgetRefreshWorkerSourceTest) that md2-0's hook change broke.

## Before state
The widget reply affordance (my earlier display render) showed a tap-to-app "Tap to reply" for all pending-dialog methods. md2-0 then landed the PicoDialogReplyReceiver (9e3df72176) — the out-of-process confirm/cancel send-path — and filled the worker's writePicoForAgent hook (f2db9d54f4), which changed its signature and broke my AgentWidgetRefreshWorkerSourceTest exact-signature pin.

## After state
- AgentStatusWidgetProvider: when data.pendingDialogMethod == "confirm", shows direct Confirm/Cancel buttons wired via explicit PendingIntent.getBroadcast to PicoDialogReplyReceiver — Confirm = ACTION_CONFIRM + EXTRA_CONFIRMED true, Cancel = ACTION_CANCEL, with EXTRA_AGENT_ID/EXTRA_DIALOG_ID and a distinct requestCode per agent/dialog/action (FLAG_IMMUTABLE|FLAG_UPDATE_CURRENT). The receiver opens a transient session, posts replyConfirm/replyCancel, disconnects, and clears the cached dialog + updateAll so the affordance vanishes. value/input/editor keep the tap-to-app path; blank shows none.
- agent_status_widget.xml: a confirm/cancel button row (agent_widget_reply_buttons).
- Fixed AgentWidgetRefreshWorkerSourceTest: durable token pin ("writePicoForAgent") instead of the exact signature md2-0 refactored (added a ConnectionManager param + suspend).

## Diff summary
Landed squash-merged on main — see the reintegration receipt for the final SHA. Edits: widgets/AgentStatusWidgetProvider.kt (method-gated buttons + replyBroadcast helper), res/layout/agent_status_widget.xml (button row), AgentWidgetRefreshWorkerSourceTest.kt (durable pin); new AgentWidgetConfirmButtonsSourceTest.kt.

## Embedded artefacts
- Full :app:testDebugUnitTest: 1886 tests, 0 failures+errors.
- AgentWidgetConfirmButtonsSourceTest 4/0; AgentWidgetRefreshWorkerSourceTest 3/0 (fixed).
- :app:assembleDebug success.

## Operator-takeaway
bd-207574 is COMPLETE: you can now Confirm/Cancel a pico agent's blocking dialog one-tap straight from the home-screen widget (no app open), via md2-0's out-of-process receiver. This closes the whole reply-from-widget feature, delivered conflict-free across the 3-way handoff (md2-0 cache + send-path; md2-1 render + buttons; msd-1 reply controls). md2-0 will verify the buttons on-device during their bd-257184 capture. Also fixed a broken-on-main test caused by md2-0's hook-signature refactor (durable token pin, per the exact-pins-break lesson).
