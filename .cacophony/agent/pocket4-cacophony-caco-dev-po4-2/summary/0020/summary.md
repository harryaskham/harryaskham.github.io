# Session summary — intermittent ms-mac TTS failures now surface loudly in status and trace

## Goal

Address the remaining ms-mac TTS health gap after routing semantics were clarified: the daemon could still be running and unmuted while intermittent `/api/v1/audio/speech` failures accumulated in the background. The goal was to make those failures visible and actionable without destructive remediation, so operators can distinguish “TTS daemon is alive” from “speech requests are currently succeeding.”

## Bead(s)

- `bd-ba1a3b` — ms-mac TTS: intermittent speech API 500 failures observed in trace

## Before state

- ms-mac-local agents had already verified daemon-side health and trace rows with `outcome=played`.
- ms-mac local probes showed:
  - daemon running
  - unmuted
  - output routed to `local-default`
  - some recent traces with `outcome=played`
- But the same investigation also surfaced growing intermittent failures:
  - upstream `speech API error: 500 Internal Server Error`
  - local request errors reaching `http://127.0.0.1:11100/api/v1/audio/speech`
- The TTS daemon runtime status only exposed aggregate counters (`total_failures`, etc.), not the most recent failure summary.
- In the main playback loop, provider-side non-2xx responses were summarized only as `speech API error: <status>` with no response-body detail, model, voice, or message-size context.
- Harry kept repeating the audible-playback directive, so a superficially healthy daemon was not enough signal.

## After state

- The TTS daemon runtime now retains:
  - `last_failure_at`
  - `last_failure_summary`
- `GET /api/v1/tts/status` now exposes those fields, and the human-facing `caco tts status` output prints them.
- Provider-side non-success responses now record a richer summary including:
  - status code
  - current model
  - effective voice
  - message length
  - truncated upstream response body when available
- Local request failures to `/api/v1/audio/speech` now also record a richer summary including:
  - timeout-window context
  - model
  - voice
  - message length
  - truncated request error detail
- `caco tts status` can now answer:
  - whether the daemon is running
  - whether the route is local-only vs operator-audible (from prior `bd-f79294` work)
  - and what the **latest failure actually was**
- This makes intermittent failure bursts operator-visible even when the daemon remains running/unmuted and some speaks still play successfully.

## Diff summary

- Files touched:
  - `crates/caco-cli/src/lib.rs`
- Validation:
  - `cargo build -p caco-cli`
  - `cargo test -p caco-cli tts_output_audibility -- --nocapture`
  - `cargo test -p caco-cli tts_setting_commands_missing_values_use_discoverability_hints -- --nocapture`
- Behavioural delta:
  - intermittent TTS failures are no longer hidden behind a superficially healthy daemon status
  - trace/status now preserve the latest failure context operators need during live ms-mac health incidents

## Operator-takeaway

The ms-mac TTS problem had two layers: routing semantics and intermittent speech failures. Routing is now explicit, and this patch makes the second layer loud: even when some speaks still play, operators can now see the most recent failure summary directly in status output instead of inferring trouble from repeated silence and aggregate counters alone.
