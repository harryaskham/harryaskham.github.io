# Session summary — bd-160b14: Android device-Assistant foundation (ACTION_ASSIST -> chat)

## Goal
Make Cacophony selectable as the Android device Assistant so the system assist gesture /
assistant shortcut opens the app (routed to chat). Android foundation slice of bd-160b14.

## Bead(s)
- bd-160b14 (claimed). This lands the Android FOUNDATION; remaining scope noted below.

## Before/After state
- Before: the app could not be selected as the device Assistant (no ACTION_ASSIST handler).
- After: the app declares an ACTION_ASSIST intent filter (which qualifies it for ROLE_ASSISTANT,
  so the user can pick Cacophony as the device Assistant), the assist launch routes to the chat
  surface, and Settings has a "Device Assistant" section with a "Set as device Assistant" button
  that opens the assistant-role / voice-input settings.

## Diff summary
- Code/content commits: pending reintegration receipt SHA.
- Files: MainActivity.kt (assistNavTargetForAction/assistNavTarget helper + onCreate/onNewIntent
  route ACTION_ASSIST -> navigationTarget "chat"); AndroidManifest.xml (ACTION_ASSIST intent filter
  on MainActivity); ui/settings/SettingsScreen.kt (AssistantSettingsSection + openAssistantRoleSettings
  with RoleManager ROLE_ASSISTANT request -> VOICE_INPUT_SETTINGS -> general settings fallback chain);
  test/AssistantNavTest.kt (2 tests). Android-only — no daemon work; uses the existing nav mechanism.

## Embedded artefacts
- 2/2 AssistantNavTest (ACTION_ASSIST -> "chat"; other actions/null -> null). assembleDebug green.
- RENDER-VALIDATED on emulator-5554: `am start -a android.intent.action.ASSIST` reached MainActivity
  (manifest filter works) and the app opened on the Chat tab (Chat selected in bottom nav, Chat screen
  shown); the Settings "Device Assistant" section renders with the "Set as device Assistant" button.
  Screenshots in file-cache: bd-160b14-assist-routes-to-chat.png, bd-160b14-device-assistant-settings.png.

## Operator-takeaway
Cacophony can now be set as the Android device Assistant (Settings -> Device Assistant -> Set as
device Assistant, or the system assist-app picker), and the assist gesture opens the chat. REMAINING
(follow-up slices, not in this landing): (1) configurable assist mapping (route the assist to a chosen
agent / the router / a dedicated daemon /assistant endpoint rather than the default chat); (2) the
optional stateful daemon /assistant endpoint (daemon/Rust); (3) the WearOS assistant surface. Bead kept
open for those.
