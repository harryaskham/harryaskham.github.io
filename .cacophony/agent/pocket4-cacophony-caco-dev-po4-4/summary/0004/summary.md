# bd-7860fd: Android per-agent audio controls

## Goal

Expose the daemon's per-agent + global audio mute state in the
Android companion app so operators can see, per running agent,
whether audio is active or silenced and why.

## Bead(s)

- bd-7860fd (this commit)
- bd-677125 (daemon read endpoint, prerequisite, on main)
- bd-42e5a3 (daemon audio proxy gate, prerequisite, on main)
- bd-46eb2b (speak handler gate, prerequisite, on main)
- bd-b2b40b (schema gate, prerequisite, on main)

## Before state

Android `SpeechControlsScreen` only exposed global TTS mute via
`/api/v1/tts/mute` / `/api/v1/tts/unmute` (bd-acb7e4). There was no
visibility into per-agent `PerAgentAudioConfig.mute` or
`AudioGlobals.global_mute` state — i.e. no way to answer "why is
this agent not speaking?" from the app.

## After state

`ConnectionManager`:
- `AgentAudioEntry`, `AgentAudioSnapshot` data classes mirror the
  JSON shape of `GET /api/v1/speech/agent-audio` (bd-677125)
- `AgentAudioSnapshot.fromJson()` deterministically sorts agents by
  project then id so UI ordering is stable
- `getSpeechAgentAudio()` suspend fn fetches + parses, returning
  `null` on transport/parse failure (same convention as
  `getTtsStatus`)

`SpeechControlsScreen`:
- `refreshStatus()` now also populates `agentAudio` state
- New card under the TTS status card lists each running agent with
  a mute icon, agent id, and a reason line:
    * `global mute` when globalMute is true
    * `agent mute` when per-agent mute is true
    * `<project> • <voice>` otherwise
- Card header toggles between `VolumeUp/AuroraPurple` and
  `VolumeOff/AuroraRed` based on globalMute
- Empty-state copy: `No running agents.`

## Diff summary

- `ConnectionManager.kt`: +73 lines (suspend fn + 2 data classes with
  `fromJson` companions + stable sort)
- `SpeechControlsScreen.kt`: +79 lines (agent-audio card + state
  hook; no new imports, reuses existing icon/theme primitives)
- `AgentAudioSnapshotTest.kt`: +120 lines (5 tests covering globals,
  per-agent mute, empty map, default fields, sort stability)
- Rust workspace unaffected; `cargo check --workspace --tests` clean.
- Android tests not runnable from worker path (no gradle in nix
  shell); tests are well-formed and follow the AppStateStoreTest
  pattern — CI / `nix run .#e2e-test` will exercise them.

## Operator-takeaway

App now shows exactly which agent is / isn't audible and why, which
matches the daemon's single-source-of-truth enforcement landed in
the bd-b2b40b → bd-46eb2b → bd-42e5a3 sequence. Mutation from the
app (toggle global or per-agent mute via REST) is deferred — runtime
mutation would need write-through-to-TOML + hot-reload plumbing, and
today a mute flip requires a `caco config` edit. The read surface
already closes the "is this agent silent by design?" ambiguity the
operator called out.
