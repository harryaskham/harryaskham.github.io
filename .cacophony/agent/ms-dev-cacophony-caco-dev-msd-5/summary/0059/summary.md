# Session summary — Android chat shows all messages by default

## Goal

Make the Android companion Chat tab show all available chat messages immediately on load instead of silently narrowing the view or omitting message bodies. The slice stays focused on the default-history behaviour for `bd-4de7e7`, leaving broader channel-chip/web-parity work to `bd-582ecf`.

## Bead(s)

- `bd-4de7e7` — Display all messages by default in Android chat

## Before state

- Failing tests: no confirmed failing code test; initial validation attempts used wrong Android Gradle entrypoints and failed before executing the relevant tests.
- Relevant metrics: Android Chat initialized `localSelectedProject` to the first discovered project, so the initial view hid other projects' messages. Chat history requests used daemon defaults, which cap at 100 and omit bodies unless requested.
- Context: A peer owns the broader `bd-582ecf` Android chat web-channel UX work, so this change needed to stay narrow and preserve an All Projects default that peer work can build on.

## After state

- Failing tests: none known for this slice.
- Relevant metrics: focused queued Android unit validation passed: `tj-fbec09dd` ran `cd companion/android && nix develop . --command gradle :app:testDebugUnitTest --tests com.cacophony.companion.ChatScreenTest`.
- Context: Android Chat now leaves the initial scope on All Projects, requests `limit=5000&include_body=true` for chat history, and parses both legacy array envelopes and daemon `data.messages` envelopes.

## Diff summary

- Commits: `07c25835f6`
- Files touched: `companion/android/app/src/main/java/com/cacophony/companion/connection/ConnectionManager.kt`, `companion/android/app/src/main/java/com/cacophony/companion/state/AppStateStore.kt`, `companion/android/app/src/main/java/com/cacophony/companion/ui/chat/ChatScreen.kt`, `companion/android/app/src/test/java/com/cacophony/companion/ChatScreenTest.kt`, `companion/android/app/src/test/java/com/cacophony/companion/TestDaemonServer.kt`
- Tests: added 3 focused ChatScreenTest regressions plus query-tolerant test-server routing; validation `tj-fbec09dd` passed.
- Behavioural delta: The initial Android Chat view is the global All Projects channel rather than the first project, and its history fetch asks the daemon for full message bodies across a much larger bounded history window.

## Operator-takeaway

The Android companion should no longer appear to drop or hide older/cross-project chat messages on first open; broader channel UX can now layer on top of a safe all-messages default.
