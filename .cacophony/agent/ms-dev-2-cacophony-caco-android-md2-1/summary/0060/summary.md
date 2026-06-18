# Session summary — bd-8736e6 slice b4: keep-alive foreground service

## Goal
Keep the embedded mic server alive while the companion app is backgrounded (bd-8736e6 slice b) — a microphone foreground service so the remote-STT stream doesn't get killed.

## Bead(s)
- bd-8736e6 (claimed; multi-slice). This lands slice b4 (the service); bd-8736e6 stays OPEN (slice b5 = the Settings toggle UI + the shared daemon-side android:// STT input-source resolver). wearOS watch-mic remains msd-4's lane.

## Before state
slices b1-b3 landed the contract, the AudioRecord capture engine, and the embedded HTTP PCM server. The server only ran while something held it; there was no foreground keep-alive for background streaming.

## After state
- New audio/RemoteMicForegroundService.kt: a foregroundServiceType=microphone Service that runs RemoteMicServer for the configured port (START_STICKY), posts an ongoing low-importance notification (statusText surfaces the port + STT purpose), creates its notification channel, and exposes start(context)/stop(context) helpers (startForegroundService on O+). stop() tears the server down on destroy.
- AndroidManifest.xml: FOREGROUND_SERVICE + FOREGROUND_SERVICE_MICROPHONE permissions + the RemoteMicForegroundService registration (exported=false, foregroundServiceType=microphone).
- RemoteMicForegroundServiceTest: status-text content, channel/notif-id constants, and a manifest source-pin for the microphone foreground-service registration + permission.

## Diff summary
Landed squash-merged on main — see the reintegration receipt for the final SHA. New file: audio/RemoteMicForegroundService.kt; manifest perms + service registration; RemoteMicForegroundServiceTest.kt.

## Embedded artefacts
- Full :app:testDebugUnitTest: 1903 tests, 0 failures+errors.
- RemoteMicForegroundServiceTest 3/0.
- :app:assembleDebug success.

## Operator-takeaway
Slice 4 of the phone-mic-as-remote-STT feature: the microphone foreground service keeps the embedded mic stream alive while backgrounded. The engine + server + keep-alive are now in place. Remaining: slice b5 — a Settings toggle to start/stop the service and surface the android://<phone-ip>:<port> URL + bearer token, plus the shared daemon-side android:// STT input-source resolver (Rust/daemon lane, coordinated with the iOS/watchOS siblings). bd-8736e6 stays open.
