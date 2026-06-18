# Session summary — bd-8736e6 slice b3: embedded HTTP PCM-stream server + bearer auth

## Goal
Stream the captured phone mic off-device to a remote caco-STT client (bd-8736e6 slice b) — an embedded HTTP server a remote agent GETs to receive the live mic PCM.

## Bead(s)
- bd-8736e6 (claimed; multi-slice). This lands slice b3; bd-8736e6 stays OPEN. wearOS watch-mic slice (a) remains msd-4's lane.

## Before state
slice b1 landed the contract/config; slice b2 landed the AudioRecord capture engine. There was no server to expose the captured frames to a remote client.

## After state
- New audio/RemoteMicServer.kt:
  - RemoteMicHttp (pure, testable): parseRequest (method/path/Bearer extraction), isAuthorized (constant-vs-bearer, rejects blank/missing), isMicRequest (GET + /mic, case-insensitive, query-tolerant), and the 200 audio/L16 / 401 / 404 response header constants.
  - RemoteMicServer: raw-ServerSocket server (mirrors AndroidRemoteCommandServer's accept-loop + per-client-thread pattern, no server-WS dep). A remote agent GETs http://<phone-ip>:<port>/mic with Authorization: Bearer <token>; on success it streams 200 audio/L16 (16 kHz mono PCM16) from MicCaptureEngine until the client disconnects; 401 unauth / 404 other. Binds all interfaces; the bearer token is the access control.
  - Client-disconnect is signaled via an AtomicBoolean set on the capture thread's write failure, and the engine is stopped from the HANDLER thread — avoiding a self-join deadlock (MicCaptureEngine.stop joins the capture thread).
- RemoteMicHttpTest: request parsing, auth (incl. blank/missing rejection), mic-request matching, response-header shape.

## Diff summary
Landed squash-merged on main — see the reintegration receipt for the final SHA. New files: audio/RemoteMicServer.kt, RemoteMicHttpTest.kt.

## Embedded artefacts
- Full :app:testDebugUnitTest: 1900 tests, 0 failures+errors.
- RemoteMicHttpTest 5/0.
- :app:assembleDebug success.

## Operator-takeaway
Slice 3 of the phone-mic-as-remote-STT feature: the embedded HTTP server now streams the phone mic (bearer-authed) so a remote caco agent can GET http://<phone-ip>:<port>/mic and receive a live 16 kHz PCM stream. Final slice (b4): a foreground service to keep it alive while backgrounded + a Settings toggle exposing the android://<phone-ip>:<port> URL + bearer token. The shared caco-side android:// STT input-source resolver is a daemon slice. bd-8736e6 remains open.
