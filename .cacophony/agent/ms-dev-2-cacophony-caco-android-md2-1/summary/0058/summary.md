# Session summary — bd-8736e6 slice b2: AudioRecord mic capture engine

## Goal
Add the microphone capture engine for the android-phone embedded mic server (bd-8736e6 slice b) — turn the phone mic into a stream of fixed-size PCM frames the embedded server (slice b3) will relay to remote caco-STT clients.

## Bead(s)
- bd-8736e6 (claimed; multi-slice). This lands slice b2; bd-8736e6 stays OPEN. wearOS watch-mic slice (a) remains msd-4's lane.

## Before state
slice b1 landed the format/wire contract + config (RemoteMicAudioContract + RemoteMicServerConfig). There was no actual mic capture component yet.

## After state
- New audio/MicCaptureEngine.kt: opens AudioRecord on MediaRecorder.AudioSource.MIC at the contract format (16 kHz mono PCM16), reads PCM on a background thread into fixed RemoteMicAudioContract.frameBytes frames, and delivers each complete frame to an onFrame callback. start() returns false if already running or AudioRecord fails to initialize; stop() joins the thread + releases the recorder. Caller must hold RECORD_AUDIO (already in the manifest).
- captureBufferSize(minBufferSize, frameBytes): pure, unit-testable buffer sizing — at least AudioRecord's reported minimum and a few frames of headroom, rounded up to a whole number of PCM frames.
- MicCaptureEngineTest: frame-aligned buffer math (incl. error/zero-min fallback + the >= and %frame invariants), default frame ms, and a source-pin that the engine captures at the contract format.

## Diff summary
Landed squash-merged on main — see the reintegration receipt for the final SHA. New files: audio/MicCaptureEngine.kt, MicCaptureEngineTest.kt.

## Embedded artefacts
- Full :app:testDebugUnitTest: 1895 tests, 0 failures+errors.
- MicCaptureEngineTest 3/0.
- :app:assembleDebug success.

## Operator-takeaway
Slice 2 of the phone-mic-as-remote-STT feature: the capture engine that produces 16 kHz mono PCM16 frames from the phone mic. Next: (b3) a raw-ServerSocket HTTP chunked-PCM stream server with bearer auth (mirroring AndroidRemoteCommandServer's pattern) that streams these frames to a remote caco agent, then (b4) a foreground service + a Settings toggle exposing the android://<phone-ip>:<port> URL. The shared caco-side android:// STT input-source resolver is a daemon slice. bd-8736e6 remains open (multi-slice).
