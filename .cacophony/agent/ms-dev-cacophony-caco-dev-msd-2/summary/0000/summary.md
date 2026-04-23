# Session summary — bd-4552dc operator-readable STT error UX + doctor

## Goal

STT failures today land as opaque stack traces (numpy ABI clashes,
"model file not found", `pulseaudio device not present`). This bead
owns the **classification + actionable-message** layer so every
known failure mode maps to a one-line operator-facing fix with an
optional platform-specific hint and docs URL. Plus the
`caco stt --doctor` report shape, model-download prompt, and
sample-test result types.

## Bead(s)

- `bd-4552dc` — STT operator-readable error UX + onboarding (P2)
- (parent epic `bd-9496d1` STT hardening)
- (specifically catches the bd-b69525 Python ABI clash class)
- (sibling to bd-c1930c grammar bias I landed earlier this session)

## Before state

- caco-stt-protocol crate carried streaming wire types + grammar
  hints. No error-classification layer.
- Future engine wrapper (bd-71ce98) would have surfaced raw stack
  traces directly to operators.

## After state

- New `crates/caco-stt-protocol/src/doctor.rs` (~530 lines)
  registered as `pub mod doctor` in lib.rs.
- `SttErrorClass` enum (11 variants): MicUnavailable, MicSilent,
  ModelMissing, ModelCorrupt, EnvConflict, EngineMissing,
  EngineCrashed, AudioFormat, Network, DiskIo, Unknown.
- `SttError { class, headline, fix_hint?, docs_url?, raw? }` —
  builder pattern with `with_fix`, `with_raw`, `with_docs`.
- `SttPlatform { Linux, MacOs, Windows, Android }` with
  `current()` cfg-detection.
- `classify_raw_error(raw, platform) -> SttError` — pure-text-in,
  pure-record-out classifier covering all 11 known failure modes
  (specifically the bd-b69525 `_multiarray_umath` / `numpy.core`
  pattern, mic-permission, no-input-device, model-not-found,
  model-corrupt, engine-binary-missing, sample-rate, network,
  disk-full, read-only-fs, engine-crashed, and unknown-fallthrough
  with a `--doctor` hint).
- `platform_mic_permission_fix(p)` — one-line per OS:
  - Linux: pavucontrol / wpctl status
  - macOS: System Settings → Privacy & Security → Microphone
  - Windows: Settings → Privacy & security → Microphone
  - Android: companion intent dialog
- `platform_mic_setup_fix(p)` — same per-OS for "no mic detected"
- `platform_hint_for(class, p)` — single entry for UI surfaces
- `SttDoctorReport { schema_version, platform, generated_at,
  probes: Vec<DoctorProbe> }` with `has_failures()` + `headline()`
  ("all OK" / "warnings" / "failures") for traffic-light UIs
- `DoctorProbe { label, status: ProbeStatus, detail, fix_hint? }`
- `ProbeStatus { Ok, Warn, Fail, Skipped }`
- `model_download_prompt(name, bytes)` — "Download X? (123MB) [Y/n]"
  / "Download X? (3.0GB) [Y/n]" depending on size
- `SampleTestResult` enum (tagged): `Transcript { text,
  latency_ms }` or `Failed { error: SttError }` — sample-test on
  first run never opaque-fails

## Diff summary

- Files: 2 modified — `crates/caco-stt-protocol/src/lib.rs` (+1
  module decl) — and 1 created — `src/doctor.rs` (~530 lines incl.
  tests)
- Tests: +22 / -0 (caco-stt-protocol total: 61 passing in 0.01s)
- Behavioural delta: zero — pure addition. No engine probes wired
  yet (lives in caco-daemon next to whichever engine bd-71ce98
  picks).

## Operator-takeaway

Every known STT failure mode now has a classifier path with a
one-line fix. When the operator sees:

  STT operation failed: STT Python environment clash: numpy/wrapper
  ABI mismatch
  Try: `caco stt --rebuild-venv` (re-creates the bundled Python env
  from scratch)
  See: https://docs.cacophony.local/stt/env-conflict

…they know what to do and the underlying stack trace is a single
disclosure-on-demand expander away (`raw` field).

`SttDoctorReport` is the data the `caco stt --doctor` CLI will
print. Every consumer (TUI, web, JSON output) reads the same
schema-versioned shape so the "all OK / warnings / failures"
traffic light is uniform across surfaces.

The bd-b69525 class is specifically called out by the
classification ordering — it's checked first because the surface
symptom (`ImportError on _multiarray_umath`) is otherwise easy to
mis-classify as a generic engine-crash. The fix hint
(`--rebuild-venv`) is the right action; if the operator runs it
they have a working STT in seconds rather than hours of stack-
trace debugging.

## Follow-ups noted

- Daemon-side probe implementations (run `arecord -l`, list models
  in $XDG_CACHE_HOME, run a 3-second sample) — small, separate
  bead worth filing once bd-71ce98 picks an engine.
- A per-platform "caco stt --rebuild-venv" implementation lives in
  the engine integration; this module just specifies the message
  the operator sees.
