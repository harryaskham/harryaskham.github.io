# Session summary — TTS trace now exposes authored-time ingress lag for delayed speaks

## Goal

Clarify the remaining ms-mac TTS follow-up after audible playback converged on
`local-device`: some router-originated speaks looked missing for over a minute,
then later appeared and played. The goal was to make delayed cross-node speaks
visibly delayed in `caco tts trace` instead of looking indistinguishable from
never-arrived messages, so operators can separate upstream delivery lag from
local playback failure.

## Bead(s)

- `bd-e2c81f` — ms-mac TTS feed: specific router-originated speak IDs not reaching ms-mac trace despite healthy daemon (delivery/filter gap)
- context only: `bd-fbf0ce` — operator-action audible-path verification remains a hardware/local-listener question, not something this pocket4 runtime can directly hear

## Before state

- The local TTS daemon already parsed `authored_at` from feed events.
- The trace recorded when the daemon first saw a speak (`generated`) and later
  lifecycle stages (`enqueued`, `played`, etc.), but it did not expose the
  delay between original feed authorship and local TTS receipt.
- In practice this meant a late cross-node speak could look “missing” during an
  observation window even if it later arrived and played.
- Code inspection also showed the TTS daemon subscribes to the live-only
  `/api/v1/feed/stream` view, so late appearance in trace points upstream at
  feed publication / replication latency rather than a local TTS trace filter.

## After state

- `caco tts trace` now records `detail=authored_at=... age_ms=...` on the
  `generated` stage whenever the original feed timestamp is available.
- `age_ms` is the local daemon's observed ingress lag from original feed
  authorship to local receipt.
- This makes delayed cross-node speaks visibly late, which lets operators
  distinguish “upstream lag” from “never arrived”.
- README and macOS recovery docs now explain how to interpret the new lag
  signal in `caco tts trace`.

## Diff summary

- Files touched:
  - `crates/caco-cli/src/lib.rs`
  - `README.md`
  - `docs/macos-development.md`
- Tests added:
  - `tts_trace_generated_detail_includes_authored_at_and_age_ms`
  - `tts_trace_generated_detail_is_none_without_authored_at`
- Existing tests re-run:
  - authored-at extraction tests for `tts_daemon_extract_speakable`
- Validation:
  - `cargo fmt --all`
  - `cargo test -p caco-cli tts_trace_generated_detail -- --nocapture`
  - `cargo test -p caco-cli extract_speakable_authored_at -- --nocapture`
  - `cargo build -p caco-cli`
- Behavioural delta:
  - operator-visible TTS trace now quantifies delivery latency at ingress time
  - delayed cross-node speaks no longer present as an opaque “maybe dropped” gap

## Operator-takeaway

The remaining ms-mac TTS ambiguity is mostly about observability, not the local
speaker route. The route/playback side is healthy; the missing piece was making
cross-node delay explicit. `caco tts trace` now shows authored-time ingress lag,
so when a router-originated speak arrives late the trace can prove it was late
rather than silently lost. True “Harry heard it” confirmation still requires a
local listener or a dedicated microphone-loopback bead.
