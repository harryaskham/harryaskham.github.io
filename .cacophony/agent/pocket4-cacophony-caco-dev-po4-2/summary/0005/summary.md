# Session summary — bead-close lifecycle events become audible

## Goal

Make bead-close lifecycle events actually audible through the existing
TTS daemon path. Manual `caco msg speak` was audible, but daemon-side
ambient lifecycle notifications were not: the daemon published feed
notifications, but never emitted the documented speak fallback, and
`bead.closed` was not wired into the real close paths.

## Bead(s)

- `bd-913fc1` — `[tts/daemon] Audible chime + TTS announcement on bead close and key lifecycle events`

## Before state

- Manual `caco msg speak` worked and was audible.
- `dispatch_ambient_notification()` promised "feed event + speak-text
  fallback message" in its docstring, but the implementation only
  published a feed `notification` event.
- `bead.closed` already existed in `chimes.rs` and `notifier.rs`, but
  bead close paths in `beads.rs` did not dispatch the ambient notifier.
- `bead.closed` policy defaulted to readout-off, so even if it had been
  dispatched the operator would only get a chime.

## After state

- `dispatch_ambient_notification()` is now async and does two things:
  1. publishes the feed `notification` event as before
  2. inserts a project-scoped `MessageKind::Speak` fallback row with the
     resolved readout text so the existing TTS daemon path can read it aloud
- `bead.closed` readout defaults on now, while remaining operator-overridable
  through `notifications.yaml`.
- `bead.closed` readout format is upgraded from `bead <id> closed` to
  `bead <id> closed: <title>` when a title is available.
- All actual `EventType::BeadClosed` emitters in `beads.rs` now also call
  the ambient notifier helper, including:
  - explicit `caco bd close`
  - `caco bd update --status closed`
  - stale-assignee reconciliation closeouts
  - completed-agent auto-close paths
- Fast preflight green: 185 / 185 `cargo test-small` passed.

## Diff summary

- Files touched:
  - `crates/caco-daemon/src/lib.rs`
  - `crates/caco-daemon/src/beads.rs`
  - `crates/caco-daemon/src/notifier.rs`
- Tests run:
  - `cargo test -p caco-daemon --lib notifier::`
  - `cargo test-small`
- Behavioural delta:
  - bead close paths now produce an audible TTS readout through the
    same daemon/TTS route as manual speak
  - readout includes bead ID + title by default

## Operator-takeaway

The missing audio was not a TTS daemon failure — it was a wiring gap.
The feed-side ambient notification system already existed, but the
"speak fallback" mentioned in the code comments was never actually
inserted, so lifecycle events stayed visible-only. This patch closes
that gap using the existing daemon/TTS pipeline instead of inventing a
parallel lifecycle-audio path.
