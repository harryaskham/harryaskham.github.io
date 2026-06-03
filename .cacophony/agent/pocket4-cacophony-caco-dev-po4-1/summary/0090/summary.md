# Session summary — daemon logical speech senders first-class in TTS policy/explain

## Goal

Bead-closure / cron / speaking-clock speech notifications were unverifiable and
effectively inaudible because the daemon's synthetic "logical senders"
(`<node>:daemon:notifier`, `:cron`, `cron:speaking-clock`, `router`, ...) had no
first-class identity in the speech policy or diagnostic surfaces. The operator
could not answer "why didn't I hear that bead closure?" the way they can for a
managed agent: `caco tts status --explain "helsinki:daemon:notifier"` errored
with "live agent not found in speech/agent-audio". This session made logical
senders first-class in the policy/explain/status path so their effective
route/mute/priority is introspectable and they are not silently mis-prioritized.

## Bead(s)

- `bd-c2f435` — Daemon logical speech senders (notifier/cron/speaking-clock) are
  not first-class in TTS policy/explain (P2 bug)
- Filed (unclaimed, out of lane): `bd-7bcfcd` — pre-existing test-only clippy
  `-D warnings` errors in unrelated caco-daemon test files (broken-on-main, P3)
- Related (not in scope, separate half): `bd-d9d11e` — playback-stall reap

## Before state

- Failing tests: none attributable to this area; the symptom was a runtime/UX
  gap, not a failing test.
- `caco tts status --explain "<node>:daemon:notifier"` -> error
  "live agent not found in speech/agent-audio".
- `/api/v1/speech/agent-audio` exposed only live-agent entries; logical senders
  were invisible to policy introspection.
- Resolver `resolved_agent_audio_with_context` was general-purpose but never
  invoked for logical senders.

## After state

- Failing tests: none introduced. 8 new caco-config unit tests pass.
- `caco-config`: new `LogicalSender::parse` / `looks_logical` + new
  `SpeechConfig::resolved_logical_sender_audio` resolve a logical sender through
  the existing by_node / by_agent_role / default chain, returning a
  `ResolvedAgentAudio` comparable to an agent's.
- `caco-daemon`: `/api/v1/speech/agent-audio` now includes a `logical_senders`
  map (known `<node>:daemon:<role>` roles + `cron:speaking-clock` + `router`)
  with mute/global_mute/silenced/node/role/matched_rule.
- `caco-cli`: `tts status --explain` falls back to the logical-sender entry
  instead of erroring, and reports a `kind:` (agent vs logical_sender) line;
  JSON gains `sender_kind`.
- Validation (queued on shared host):
  - `cargo test -p caco-config logical_sender_audio_tests_bd_c2f435` — 8 passed.
  - `cargo check -p caco-daemon -p caco-cli --tests` — exit 0.
  - `cargo clippy -p caco-config -p caco-daemon -p caco-cli` (non-test, matching
    the cacophony-fast-tests workspace gate) — clean.

## Diff summary

- Code commit: `48e230a15` (final landed squash SHA will come from the
  reintegration receipt).
- Files touched: `crates/caco-config/src/model.rs`,
  `crates/caco-daemon/src/lib.rs`, `crates/caco-cli/src/lib.rs`.
- Tests: +8 (caco-config logical-sender parse + resolution).
- Behavioural delta: daemon logical speech senders are now introspectable
  through the standard policy/explain/status surfaces; `--explain` resolves
  effective route/mute/priority instead of erroring; closure/cron/clock speech
  is no longer opaque to policy. The playback-stall (sink-drain) half stays
  tracked separately by bd-d9d11e.
- Also fixed one pre-existing `field_reassign_with_default` clippy lint in a
  TUI-lens test in the same file; remaining unrelated test-only clippy lints
  filed as bd-7bcfcd.

## Operator-takeaway

You can now run `caco tts status --explain "<node>:daemon:notifier"` (or
`cron:speaking-clock`, `router`, etc.) and get a real route/mute/priority answer
with a `kind: logical_sender` marker, instead of "live agent not found". This
closes the diagnostics/policy half of the inaudible-bead-closure problem. If
closures are still silent under non-muted conditions, the remaining suspect is
the playback-path sink stall on the phone PulseAudio output — tracked by
bd-d9d11e, which is a separate landing and (per the bead) not yet on the sgu24
build.
