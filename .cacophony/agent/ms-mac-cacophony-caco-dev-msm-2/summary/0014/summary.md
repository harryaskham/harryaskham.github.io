# Session summary — gpt-realtime integration research (bd-fbc9e9)

## Goal

Document how the operator's standalone `tts` Python CLI tool
integrates OpenAI's `gpt-realtime` WebSocket model so the
follow-up implementation bead (which adds gpt-realtime to
Cacophony's LLM framework) doesn't reinvent the wire protocol
from scratch.

## Bead(s)

- `bd-fbc9e9` — Review 'tts' CLI tool source code for
  gpt-realtime-1.5 usage (P2, gpt-realtime, integration,
  research)

## Before state

- The operator's `~/mono/tools/tts/src/tts/__main__.py`
  (~3000 LOC) has a working gpt-realtime integration but no
  Cacophony-side documentation captures its patterns.
- An implementation bead would have started by re-reading
  3000 LOC of Python or guessing the wire protocol.

## After state

New `docs/research/bd-fbc9e9-gpt-realtime-integration-patterns.md`
(~10KB, 14 sections) covers:

- **Reference tool overview** — path, language, default model
  env var, two operating modes (one-shot vs streaming
  session)
- **WebSocket URL construction** — full code, base-url
  reuse, model in query string, /v1/realtime path
- **Connection headers** — `OpenAI-Beta: realtime=v1`,
  bearer token, subprotocols `[realtime,
  openai-beta.realtime-v1]`
- **5-stage session lifecycle** — connect → session.created →
  session.update → session.updated → history replay
- **Send-text / receive-audio message shapes** —
  `conversation.item.create` with `input_text` vs the chat-
  completions `text` content kind; cross-path session-history
  pollution warning
- **Stream event-loop table** — 5 event types with payload +
  action
- **Audio format** — PCM16 / 24kHz / mono; 150-300ms
  pre-buffer for device playback, bypass for file/stdout
- **Voice catalog** — standard + realtime-only voices;
  recommendation for per-model voice discovery
- **Auth + config** — `OPENAI_API_KEY`, `OPENAI_BASE_URL`
  proxy reuse, `OPENAI_REALTIME_MODEL`
- **Error handling** — `InvalidStatusCode` 101-upgrade
  failures, in-band server errors, recommend bd-fca3e1
  unified-error envelope on Cacophony side
- **6 Cacophony integration recommendations** — reuse
  litellm-proxy template-value flow, encapsulate
  RealtimeSession type, copy AudioBuffer pattern, extend
  /api/v1/tts/voices with `?model=` query, session-history
  file-format compat with the reference tool, model-selection
  not `--realtime` flag
- **Out-of-scope follow-ups** — VAD (STT lane bd-9496d1),
  audio input, function calling, Azure specifics
- **Source citations** — 9 file:line references for re-review
  against future revisions

## Diff summary

- Files touched:
  - `docs/research/bd-fbc9e9-gpt-realtime-integration-patterns.md`
    (new, ~10KB)
- Tests: +0 / -0 (research bead, no code)

## Operator-takeaway

When the implementation bead is filed, link this doc and the
6 integration recommendations are the spec for how it should
land. Key recommendations to flip-or-keep BEFORE writing code:

1. **Reuse litellm-proxy template-value flow** — Cacophony's
   existing `image generate` / `audio synthesize` use it; the
   realtime endpoint is the same proxy; only swap
   `https://` → `wss://`. No new auth path.
2. **Encapsulate session lifecycle** — `RealtimeSession`
   typed wrapper with explicit states (the reference tool's
   flat async functions are fine for a CLI; Cacophony wants
   the typed wrapper for testability).
3. **Audio buffer parity** — copy the `_AudioBuffer`
   pre-buffer-then-stream pattern (150-300ms before device
   playback, bypass for file/stdout). The TUI's existing
   speech path already has the device pieces.
4. **Per-model voice catalog discovery** — extend
   `/api/v1/tts/voices` with `?model=` so `validate_tts_voice`
   (bd-205b39) rejects realtime-only voices when the target
   model is non-realtime and vice versa.
5. **Session-history file-format compat** — match the
   reference tool's `~/.local/state/tts/sessions/<name>/
   session.json` shape so operators can move sessions between
   the standalone tool and `caco`.
6. **Don't gate behind `--realtime` flag** — Cacophony has
   the clean slate to make MODEL the only knob (gpt-realtime
   vs gpt-4o-mini-tts vs gemini-...) and route to WS or REST
   automatically per-model capability.

Honored constraints:
- No code changes; pure research artefact under `docs/research/`.
- No `cargo test`.
- Operator no-narrator rule honored — claim + close speaks
  issued by msm-2 directly.
- Operator no-docker note unaffected (research artefact).

17th bead closed this session (cumulative). 10th in this turn.

This is a NEW directory `docs/research/` — distinct from
`docs/epics/` (which holds my 4 design docs this session).
Research artefacts that capture external-tool patterns or
prior-art reviews land here; designs that pin Cacophony's own
contracts land in `docs/epics/`.
