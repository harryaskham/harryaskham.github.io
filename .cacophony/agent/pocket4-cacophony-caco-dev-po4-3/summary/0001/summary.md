# Session summary — bd-5ac69b per-agent audio settings schema

## Goal

Land the per-agent audio settings schema (mute, voice, voice_filter,
speed) in `caco-config` so the downstream audio-control bead cluster
(bd-46eb2b mute hierarchy, bd-cf00b9 TUI controls, bd-677125 web,
bd-7860fd Android, bd-42e5a3 daemon integration) and the schema-gate
P0 bd-b2b40b have a typed foundation to build on. Per coordination
with po4-4 (option (a)): land the data-model layer here; po4-4 layers
GlobalMute + validate.rs entries + persistence + precedence tests on
top under bd-b2b40b.

## Bead(s)

- `bd-5ac69b` — Design per-agent audio settings schema
- (parent context: `bd-b2b40b` — P0 schema gate; po4-4 will extend
  this work with GlobalMute + validation + daemon wiring)
- (downstream: `bd-46eb2b`, `bd-cf00b9`, `bd-677125`, `bd-7860fd`,
  `bd-42e5a3`)

## Before state

- Failing tests: none.
- Relevant metrics: `crates/caco-config/src/model.rs` had no
  per-agent audio override types. `SpeechConfig` exposed only
  global `mute` (`MutePolicy`), global `tts`, global `stt`. There
  was no typed way to say "mute caco-dev-msd-5 only" or "give caco-tui
  the Sage voice".
- Context: 5 downstream audio beads were blocked on schema absence.
  po4-4 had unclaimed bd-46eb2b + bd-cf00b9 with notes pointing at the
  missing schema; caco-ctrl filed bd-b2b40b as P0 schema-gate.

## After state

- Failing tests: none. `cargo test -p caco-config --lib` reports
  760 passed (was 754 — 6 new tests added).
- Relevant metrics: new types `PerAgentAudioConfig` and
  `AgentAudioOverride` added to `caco-config::model`; new
  `SpeechConfig.agent_audio: Option<PerAgentAudioConfig>` field;
  `SpeechConfig::overlay()` extended to merge `agent_audio` maps by
  agent id (overlay-wins-on-conflict); schema field expectation
  table updated; schema leaf entry added so the canonical schema
  reflects the new field; helper methods (`get`, `is_muted`,
  `voice_for`, `voice_filter_for`) provided so consumers don't
  reach into the HashMap directly.
- Context: bd-b2b40b's AC1 (per-agent struct), AC3 (default-merge
  precedence — agent override > global, documented in field rustdoc
  and exercised in overlay tests), AC6 (unit tests), and AC7
  (migration: all new fields `Option<>`, existing configs load
  unchanged) are satisfied. Po4-4 will add AC2 (GlobalMute), AC5
  (validate.rs entries), and AC4 (daemon-side persistence wiring)
  on top.

## Diff summary

- Files touched: `crates/caco-config/src/model.rs` (single file,
  +304 lines / -1 line).
- Tests: +6 unit tests covering empty config, mute override,
  partial-field overrides, voice_filter fallback to global, overlay
  merge by id with overlay-wins, one-sided overlay (`base` has,
  `overlay` doesn't and vice versa).
- Behavioural delta: zero behaviour change for existing configs.
  New optional field; new types are dead code until consumers wire
  them in (planned for the downstream bead cluster).

## Embedded artefacts

(None — pure code change, no artefacts worth recording.)

## Operator-takeaway

Coordination with po4-4 worked as intended: when po4-4's new P0
bd-b2b40b appeared 10 minutes after I claimed bd-5ac69b, I sent the
overlap to po4-4, they chose option (a) — land my data-model layer
on main first, they layer GlobalMute + validation + persistence on
top. Net result: no duplicate work, both beads make progress, the
audio-control cluster of 5 downstream beads becomes implementable as
soon as bd-b2b40b lands. The data model deliberately keeps every
field `Option<>` so partial overrides don't force operators to
re-state defaults — important for an audio-config surface where
people will only ever touch one or two agents at a time.
