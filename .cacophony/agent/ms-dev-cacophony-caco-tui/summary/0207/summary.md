# Session summary — daemon device-mic STT input (bd-602f8f): android:// / iphone:// / watchos:// URIs

## Goal

Take a capable cross-lane bead per Harry's all-hands directive (idle dev workers
pitch in on the stuck specialist backlog with work the node can actually run).
caco-tui's own lane was drained and caco-web/caco-android were well-staffed, so I
took bd-602f8f — a daemon/CLI-Rust feature (no device access; the companion app
sides are done) wiring the `android://` / `iphone://` / `watchos://` remote
device-mic URIs into the STT capture path so `caco audio transcribe --live` can
consume a companion phone/watch mic as a remote STT source.

## Bead(s)

- `bd-602f8f` — Daemon: resolve android:// / iphone:// / watchos:// remote
  device-mic URIs as STT input sources. [claimed + implemented this session]
- sibling (done, untouched): `bd-8736e6` (Android phone server),
  `bd-4b726c` (iOS/watchOS) — they define the fixed wire format.

## Before state

- No daemon-side consumer for the companion device-mic stream. The feature had
  been pre-designed (msm-3/msd-5) but deferred as a "multi-hour, fresh-rested-
  worker" feature — exactly the freshness gate Harry's directive lifted.
- Two flagged obstacles: (1) a typed `AudioRoutingMode::DeviceMic` would touch
  ~163 usages (cascade); (2) `start_live_audio_capture` returns a Child process,
  which an HTTP reqwest stream does not fit (needs a new capture model).

## After state

- Functional device-mic STT input via the **curl-bridge**, which sidesteps both
  obstacles: a separate-divert before `AudioRoutingMode` (no cascade) + a `curl`
  Child that reuses the existing Child-process capture model (no new capture
  abstraction).
- Validated: `cargo test -p caco-cli device_mic` (job tj-cff3a534, exit 0) —
  full caco-config + caco-cli + caco-tui --tests compile + both unit tests pass.
  (The reintegration gate is echo-disabled, so this real-cargo validation is the
  authoritative check.)

## Diff summary

- Code commit: c4b96383be (final landed squash SHA from the reintegration
  receipt). Summary artefact commit: intentionally omitted.
- Files: `crates/caco-config/src/model.rs` (+ `AudioInputSource::DeviceMic`
  variant), `crates/caco-cli/src/audio_cmd.rs` (parser + resolver + curl-command
  + capture + divert + tests), `crates/caco-tui/src/speech.rs` (2 match arms).
- Behaviour: `--input android://<host>:<port>` (or a `type: device_mic` named
  `speech.io.inputs` entry) now captures from the device `/mic` HTTP stream via
  `curl -sN -H "Authorization: Bearer <token>" http://<host>:<port>/mic`, feeding
  the same PCM16-LE frame pipeline as parec/cat. Token resolved from a matching
  configured `device_mic` input; a clear error when absent.
- Tests: +2 (URI parse across schemes/IPv6/bad-forms; curl command Bearer+URL).
- Non-goal: companion app sides untouched.

## Embedded artefacts

- None.

## Operator-takeaway

The companion mic-as-remote-STT loop now has its daemon-side consumer. The key
insight that made a "multi-hour fresh-worker" feature a bounded land was the
**curl-bridge**: instead of building a new reqwest-stream capture model, spawn
`curl` as the capture Child — it reuses the existing Child-process pipeline
exactly, and a separate-divert keeps it out of the AudioRoutingMode fan-out.
Remaining follow-ups (small): the device_mic config fields aren't yet in the
generated `docs/config-schema` reference (reverted to avoid a docs-regen cycle),
and bounded mid-stream reconnect (curl is currently a single GET) could be added
to mirror the SSE/transcript reconnect discipline.
