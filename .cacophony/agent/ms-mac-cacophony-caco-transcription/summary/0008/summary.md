# Session summary — transcript echo guard and narrator profile

## Goal

Update transcript-reading guidance so profiles that consume ambient STT logs understand that Cacophony TTS may sometimes be played out loud and recaptured by STT, creating feedback loops that should not be actioned as fresh operator intent. Also make the transcript bead-filer read all configured ambient STT sources instead of only the local default, and add a disabled-by-default transcript-narrator co-narrator profile that can augment fleet transcripts with spoken web/code context.

## Bead(s)

- `bd-f69d22` — Teach transcript readers to ignore TTS feedback loops
- `bd-8b18e5` — Add transcript-narrator co-narrator profile
- Follow-up filed: `bd-0d0927` — Add a scoped docs validation mode for touched Pages files

## Before state

- Failing tests: none known for this doc/profile slice.
- Relevant metrics: `bd-930017` had just been closed after ms-mac and sgu24 STT daemons produced transcript diffs; ambient transcript samples included agent/TTS-like narration.
- Context: `transcript-bead-filer` already warned against generic agent narration, but it did not explicitly call out loudspeaker feedback loops and only defaulted to one local instance. There was no dedicated transcript co-narrator profile for broad conversational augmentation. `docs/transcription.md` examples also used stale `en-US-JennyNeural`, which is not in the daemon-advertised configured voice list.

## After state

- Failing tests: none known. Targeted doc/profile checks and `git diff --check` passed. Full `docs/validate-pages.sh` progressed cleanly but exceeded a 300s local timeout.
- Relevant metrics: `caco audio capabilities --json` advertised configured voices including `en-US-Reed:MAI-Voice-1`; the docs fixture now uses that voice instead of `en-US-JennyNeural`. `caco config validate --json` passed after the declaration/profile edits.
- Context: Transcript listener policy now explicitly treats likely TTS/status loudspeaker echo as unauthenticated noise unless fresh operator intent remains after removing agent-status-like phrases. The checked-in transcript bead-filer declaration now sets `CACO_TRANSCRIPT_FILER_STT_SOURCES=ms-mac=local:ms-mac,sgu24=sgu24:sgu24`. The new `transcript-narrator` declaration sets matching narrator sources but is `auto_start: false` until intentionally enabled.

## Diff summary

- Commits: `35a1187e4`, `9529180e7` (pre-rebase originals), plus the pending `bd-8b18e5` narrator commit in this checkout
- Files touched: `.cacophony/profiles/transcript-bead-filer.md`, `.cacophony/profiles/transcript-narrator.md`, `.cacophony/profiles/caco-transcription.md`, `.cacophony/agents/cacophony_persistent.yaml`, `SPEC.md`, `docs/transcription.md`, `docs/transcription.html`, `README.md`, `AGENTS.md`
- Tests: +0 / -0 / flipped 0
- Behavioural delta: Future transcript-reader profiles are instructed to advance cursors without filing/actioning when transcript chunks look like recent Cacophony TTS feedback. The transcript bead-filer is configured/documented to poll both ms-mac and sgu24 sources with per-source cursors, the new transcript-narrator profile defines read-only earpiece-style transcript augmentation across those sources, and the transcription fixture docs now use a configured MAI voice.

## Operator-takeaway

Ambient STT remains useful even when speakers are occasionally used, but profile policy now assumes echoes can happen and requires transcript consumers to prefer silence/no-op over turning Cacophony's own spoken status back into work. The bead-filer should now see both configured ambient transcript streams after its persistent runtime is recreated or refreshed, and Harry has a concrete `transcript-narrator` profile to enable when he wants spoken conversational context rather than bead filing.
