# Session summary — bd-207574 reply-affordance display render (Android widget tap-to-reply)

## Goal
Render the reply affordance in the agent home-screen widget when a pico agent has a blocking pending dialog — the md2-1 display-render contribution to md2-0's bd-207574 (reply-from-widget).

## Bead(s)
- bd-207574 (md2-0's bead; this is the md2-1 display-render contribution — NOT closing it). Remaining on bd-207574: md2-0's part-2 out-of-process confirm/cancel BroadcastReceiver + transient daemon POST, then the upgrade from tap-to-app to direct in-widget confirm/cancel buttons.

## Before state
md2-0's part 1 landed the pending-dialog cache (AgentWidgetData.pendingDialogId/Method/Prompt populate when a pico agent has a blocking dialog), but the widget never rendered any reply affordance.

## After state
- AgentStatusWidgetProvider: when data.pendingDialogMethod is non-blank, shows the reply section (pendingDialogPrompt + a "Tap to reply" action) wired to a tap-to-app PendingIntent — navigate_to=agent:<id>, opening that agent's pico view where the existing inline/fullscreen reply controls (msd-1) handle confirm/value/cancel. When blank, the section is GONE.
- agent_status_widget.xml: a reply section (agent_widget_reply container + agent_widget_reply_prompt + agent_widget_reply_action).

This is the display + tap-to-app phase (works for every method with no part-2 dependency). Direct in-widget confirm/cancel buttons are the follow-on once md2-0's receiver action string lands.

## Diff summary
Landed squash-merged on main — see the reintegration receipt for the final SHA. Edits: widgets/AgentStatusWidgetProvider.kt (reply render + tap-to-app intent), res/layout/agent_status_widget.xml (reply section); new AgentWidgetReplyAffordanceSourceTest.kt.

## Embedded artefacts
- Full :app:testDebugUnitTest: 1871 tests, 0 failures+errors.
- AgentWidgetReplyAffordanceSourceTest 2/0 (provider gating + prompt + tap-to-app intent; layout reply section).
- :app:assembleDebug success.

## Operator-takeaway
The agent widget now surfaces a reply affordance for a pico agent's pending dialog: it shows the prompt and a tap-to-reply that opens the agent's pico view to answer. This is the conflict-free contract->render handoff continuing on bd-207574 (md2-0 owns the cache + the out-of-process send-path; md2-1 renders). bd-207574 stays open for md2-0's part-2 receiver and the subsequent direct confirm/cancel button upgrade.
