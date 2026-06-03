# Session Summary — bd-842538 (express_as.yaml stale :MAI-Voice-2-Preview suffix)

## Bead
**bd-842538** (P3 bug; oracle complexity 3/5, risk 2/5)
"express_as.yaml supported_voices still lists stale :MAI-Voice-2-Preview suffix (400s);
non-en-US v2 voices lose express-as"

Found during a helsinki controller TTS config audit after the operator switched to
MAI-Voice-2.

## Root cause
`.cacophony/tts/express_as.yaml` `ssml.azure_express_as.supported_voices` still listed the
old `:MAI-Voice-2-Preview` suffix for the four v2 voices, while the model files
(`tts/models/mai-voice-2*.yaml`) correctly use `:MAI-Voice-2`. The `-Preview` suffix
returns HTTP 400 (operator-confirmed).

Critically, the express-as voice gate in `crates/caco-daemon/src/audio.rs`
(`azure_express_as_voice_matches`) treats a **non-empty `supported_voices` list as
authoritative and ignores `voice_pattern` entirely** (confirmed in code + the
`AzureExpressAsConfig` doc-comment in model.rs). So with the stale list:
- `en-US-Harper:MAI-Voice-2` (the active operator voice) was **NOT** in the list (list had
  `:MAI-Voice-2-Preview`), and the en-US `voice_pattern` is never consulted while the list
  is non-empty — so Harper was **silently losing express-as** despite being
  operator-audible-proven end-to-end.
- The three non-en-US v2 entries matched nothing valid either.

## Fix (config-only, `.cacophony/tts/express_as.yaml`)
- Corrected the active, operator-proven voice to `en-US-Harper:MAI-Voice-2`, restoring its
  express-as styling.
- Deferred the three non-en-US v2 voices (`es-MX-Valeria`, `fr-FR-Soleil`, `de-DE-Klaus`):
  their stale `-Preview` entries are removed (they matched nothing valid) and replaced with
  commented `:MAI-Voice-2` placeholders, honoring the file's explicit contract that
  express-as styles need manual audible `caco audio speak` smoke-test proof per voice
  before enabling. Re-add the uncommented forms once each has audible proof.
- Did **not** broaden `voice_pattern` beyond the en-US anchor: that would silently enable
  all non-en-US v2 voices' express-as without the required audible proof. That scope is an
  explicit operator decision, consistent with the bead's "non-en-US v2 express-as may be
  intentionally deferred."
- Left the conservative styledegree/rotation block unchanged.

## Acceptance
- ✅ The four stale `-Preview` entries are gone; the active Harper voice uses
  `:MAI-Voice-2` and regains express-as.
- ✅ Non-en-US v2 express-as remains deferred behind audible-proof, per the file contract.
- ✅ Not breaking current synthesis (this is express-as allow-list correctness only).

## Validation
- `caco config validate` → `config valid` (the imported express_as.yaml parses + validates;
  15 nodes, 17 projects).
- Verified active (uncommented) supported_voices: the six MAI-Voice-1 voices +
  `en-US-Harper:MAI-Voice-2`; no remaining active `-Preview` suffix.
- Config-only change; no Rust touched.

## SPEC / docs
Preserves the explicit-allow-list-is-authoritative express-as contract and the file's
audible-proof discipline for adding styles/voices.

## Diff
Landed squash SHA: see reintegration receipt.
