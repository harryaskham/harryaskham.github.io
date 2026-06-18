# Session summary — bd-8736e6 slice b1: embedded mic audio-server contract

## Goal
Begin the android-phone in-app embedded microphone audio server (bd-8736e6 slice b) — the phone becomes a remote STT audio source a caco agent dials as android://<phone-ip>:<port>. Slice b1 lands the wire/format contract + config foundation.

## Bead(s)
- bd-8736e6 (claimed; [android-companion] phone/wearOS mic as remote STT). MULTI-SLICE — this lands slice b1 only; bd-8736e6 stays OPEN. wearOS watch-mic slice (a) deferred to msd-4 (md2-0 confirmed it's msd-4's WearOS domain; coordinated).

## Before state
The companion app had no in-app embedded mic server; the only phone-mic-as-STT path was the external PulseServer app (pulseaudio + PULSE_SERVER). The iOS sibling bd-4b726c specifies an embedded WS/HTTP audio server design for the no-pulseaudio surfaces; this brings the same in-app embedded-server approach to Android.

## After state
- New audio/RemoteMicAudioContract.kt:
  - RemoteMicAudioContract: fixed 16 kHz mono PCM16 format constants (caco STT's raw input), frameBytes(ms) math, micStreamUrl(ip,port)=http://<ip>:<port>/mic, remoteInputUri(ip,port)=android://<ip>:<port> (the caco-side STT remote-input form, resolved daemon-side alongside iphone://, watchos://).
  - RemoteMicServerConfig: enabled/port/bearer-token SharedPrefs with token generation + rotation.
- RemoteMicAudioContractTest: frame math (640 bytes/20ms), URL builders, hex-48 bearer token uniqueness, format constants.
- Mirrors AndroidRemoteCommandServer's raw-ServerSocket embedded-server pattern (no new server-WS dependency); RECORD_AUDIO permission already present.

## Diff summary
Landed squash-merged on main — see the reintegration receipt for the final SHA. New files: audio/RemoteMicAudioContract.kt, RemoteMicAudioContractTest.kt.

## Embedded artefacts
- Full :app:testDebugUnitTest: 1892 tests, 0 failures+errors.
- RemoteMicAudioContractTest 4/0.
- :app:assembleDebug success.

## Operator-takeaway
First slice of the phone-mic-as-remote-STT feature: the format/wire contract + persisted config. Follow-on slices: (b2) AudioRecord capture engine, (b3) ServerSocket HTTP chunked-PCM stream + bearer auth, (b4) foreground-service keep-alive + a Settings toggle showing the android://<phone-ip>:<port> URL. The shared caco-side android:// STT input-source resolver is a daemon-side slice. wearOS watch-mic is msd-4's lane. bd-8736e6 remains open (multi-slice). Also: bd-5bb8d9 (the chat chrome occlusion fix) was visually verified by po4-1 on a live pocket4 emulator this session.
