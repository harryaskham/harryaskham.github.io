# Session summary — bd-2954ba (TTS play button on inbox messages)

## Goal
Per Harry's take-any-bead / dev-workers-can-take-specialist-beads directive (relayed by caco-ctrl), help burn down the cross-lane backlog with a clean, well-specified, capability-appropriate bead. Picked bd-2954ba (Harry-created, clear acceptance criteria, unassigned, no blocking triage) — a caco-tui feature buildable + validatable on this Linux devbox.

## Bead(s)
- **bd-2954ba** — Add TTS play button to inbox and chat messages. Chat ALREADY ships a clickable bottom-border play chip (`hit_test_play_button_*` → `replay_chat_speech_by_id` → `speech.replay_speech`) plus a `p` keyboard replay; the gap was the **inbox** side. Implemented the inbox analog with identical speak behavior.

## Before / After
- **Before:** Inbox messages had no play affordance. Only chat bubbles could be read aloud (click chip or `p`).
- **After:** Each inbox item row shows a leading ▶ play glyph. Clicking the glyph, or pressing `p` on the selected inbox item, reads the message aloud via TTS using the original sender voice in immediate mode (interrupt + front-of-queue), reusing the exact chat path (`speech.replay_speech`).

## Diff summary
- `crates/caco-tui/src/state/mod.rs`: new `speak_inbox_message_by_id(id)` (mirrors `replay_chat_speech_by_id`; finds the `InboxDisplayItem`, calls `replay_speech(body, sender)`) + `speak_selected_inbox_message(project)` (resolves the selected filtered item, keyboard path).
- `crates/caco-tui/src/views/inbox.rs`: leading ▶ play glyph span on each inbox item row (accent, bold), occupying the first content columns for the mouse hit-test.
- `crates/caco-tui/src/app.rs`: (a) item-list mouse handler plays the message when the click lands on the leading glyph columns; (b) `p` key in GlobalInbox/ProjectInbox reads the selected inbox message aloud (mirrors chat's `p`).
- `crates/caco-tui/src/state/tests.rs`: 3 unit tests (found enqueues; nonexistent returns false + no enqueue; selected enqueues).

## Embedded artefacts
- Validation: queued `cargo test -p caco-tui speak_inbox` (compile + new tests) — see Diff for the landed SHA / reintegration receipt.

## Operator-takeaway
Inbox messages are now readable-aloud with a visible ▶ play button (click or `p`), identical speak behavior to chat (original sender voice, immediate-mode). Reuses the existing chat TTS path with no new TTS plumbing. Grounded reuse of the chat play template — not a blind-implement against an unclear target.
