# Technical-writer review summary

## Goal

Fill documentation gaps for three recently-shipped features found during a
proactive audit: the daemon `/assistant` endpoint, remote device-microphone STT
input, and the default STT model.

## Bead(s)

- `bd-c63005` — technical-writer documentation maintenance.
- Documents bd-346e5d (`/assistant`), bd-602f8f (`device_mic` STT input), bd-bceefc (`mai-transcribe-1.5` default).

## Before state

- `POST /api/v1/assistant` (bd-346e5d, opt-in via `CACO_ASSISTANT_ENABLED`) was undocumented.
- The `device_mic` STT input source (`android://`/`iphone://`/`watchos://`, bd-602f8f) was undocumented.
- `mai-transcribe-1.5` was documented as selectable but not as the checked-in default STT model (`.cacophony/stt.yaml`, bd-bceefc).

## After state

- `docs/api.html`: added the `POST /api/v1/assistant` row (opt-in flag, `{scope,message}` body, stateful per-scope pico session, `assistant_no_live_session` until Slice-1b).
- `docs/transcription.html`/`.md`: added the "Remote device-microphone STT input (`device_mic`)" section (in-app HTTP `/mic` server, 16 kHz mono PCM16, bearer token, `android://`/`iphone://`/`watchos://` URIs) and noted `mai-transcribe-1.5` is the checked-in default STT model.
- Validation: `./docs/validate-pages.sh` passed; transcription sibling marker refreshed.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched: `docs/api.html`, `docs/transcription.html`, `docs/transcription.md`.
- Behavioural delta: documentation only; all three contracts verified against source (`assistant.rs`, `model.rs`, `.cacophony/stt.yaml`).

## Operator-takeaway

Three shipped-but-undocumented features now have accurate docs sourced from the
implementation. The `/assistant` endpoint is documented with its opt-in and
Slice-1b caveats so the nascent state is clear.
