# Session summary — bd-e251c6: central TTS hears remote agent narration after full-state merges

## Goal

Investigate why operator-audible agent narration was not coming through even though
manual `caco msg speak` on helsinki was audible and worker profiles clearly included
`caco msg speak` guidance. The goal was to find the missing hop between remote-agent
`MessageSpeak` creation and helsinki’s central TTS daemon, then land the smallest
reliable daemon-side fix.

## Bead(s)

- `bd-e251c6` — `[tts/agents] Agent self-narration not producing audible speech to operator`

## Before state

Observed + confirmed before the fix:

- Worker profiles **do** compose `speak`:
  - `.cacophony/project.yaml` agent defaults include `speak`
  - `.cacophony/agents/base.yaml` includes `speak`
- I have been emitting `caco msg speak` at claim/ship milestones during this session.
- `handle_msg_speak` on the daemon emits a `MessageSpeak` feed event and publishes it to
  local `ui_broadcast`.
- `UiBroadcast::publish_feed_event()` synthesizes a `speech_requested` UI event for
  `MessageSpeak`.
- The TTS daemon consumes `/api/v1/feed/stream` and already treats `message_speak` as
  speakable.
- Live peer feed ingest (`/api/v1/feed/events`) already republishes accepted `MessageSpeak`
  events, and there is an existing test for the duplicate guard.

The missing path was the **full-state merge** path:

- `handle_state_full` merged peer full-state dumps into the local store.
- `merge_full_state_dump()` persisted remote `MessageSpeak` rows, but returned no signal about
  newly accepted narration events.
- `handle_state_full` only re-published a synthetic `DaemonState` health event after the merge.
- Result: if remote worker narration reached helsinki via periodic full-state convergence rather
  than direct live feed ingest, helsinki’s local TTS daemon never heard it.

## After state

After the fix:

- `merge_full_state_dump(...)` can optionally collect newly accepted `MessageSpeak` feed events
  while merging:
  - embedded snapshot chat/speech histories via `materialize_remote_histories(...)`
  - top-level `chat_history`
  - top-level `speech_history`
  - top-level `recent_events`
- `handle_state_full` now passes a collector vector and, after the store lock is released,
  re-publishes just those newly accepted `MessageSpeak` events via `state.ui_broadcast`.
- This means helsinki’s already-running TTS daemon hears remote worker narration even when it
  arrives through full-state sync instead of direct `/feed/events` live fan-out.
- The re-broadcast is intentionally narrow:
  - only `MessageSpeak`
  - only newly accepted rows
  - no broad re-broadcast of old chat/recent-event history, so no giant stale-feed pop-in

Validation:

- `cargo test -p caco-daemon merge_full_state_dump_collects_new_message_speaks_for_rebroadcast`
  — pass
- `cargo test -p caco-daemon feed_ingest_endpoint_does_not_republish_duplicate_ui_events`
  — pass
- `cargo clippy -p caco-daemon --all-targets -- -D warnings` — clean
- `cargo test-small` — 185 passed, 0 failed

## Diff summary

Files touched:

- `crates/caco-daemon/src/replication.rs`
- `crates/caco-daemon/src/lib.rs`
- `crates/caco-daemon/tests/multinode.rs`
- `.cacophony/agent/winmini-cacophony-caco-dev-wmi-2/summary/0022/summary.md`

Code changes:

1. `replication.rs`
   - `merge_full_state_dump(...)` gained an optional collector for newly accepted
     `MessageSpeak` events.
   - `materialize_remote_histories(...)` gained the same optional collector.
   - when a merged row is accepted and its type is `EventType::MessageSpeak`, the event is
     cloned into the collector.
   - added unit test:
     `merge_full_state_dump_collects_new_message_speaks_for_rebroadcast`

2. `lib.rs`
   - `handle_state_full(...)` now allocates a collector vec, passes it into
     `merge_full_state_dump(...)`, and after the merge re-publishes each accepted
     `MessageSpeak` via `state.ui_broadcast.publish_feed_event(event)`.
   - comment explains why this is intentionally scoped only to narration events.

3. `tests/multinode.rs`
   - updated existing `merge_full_state_dump(...)` callsites for the new optional collector
     parameter (`None` in those tests).

Behavioural delta:

- Remote worker narration that reaches helsinki through full-state convergence now becomes a
  live local UI/feed event again, which makes the existing helsinki TTS daemon speak it.
- No change to the duplicate-ingest guard for `/api/v1/feed/events`.
- No broad replay of stale feed history to the TUI.

## Operator-takeaway

The issue was not “workers forgot to call `caco msg speak`” — they were speaking.
The missing piece was that **full-state replication persisted remote narration but did not
re-publish it into the live local broadcast stream that the central TTS daemon listens to**.
That gap is now closed for `MessageSpeak`, so helsinki can audibly announce remote agent
self-narration without depending on perfect direct live feed fan-out.