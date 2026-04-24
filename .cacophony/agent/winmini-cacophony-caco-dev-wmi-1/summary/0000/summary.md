# Session summary — bd-241b84 Issue 3: caco audio transcribe --model upfront validator

## Goal

Pin Issue 3 of the bd-241b84 caco audio sweep: `caco audio
transcribe --model bogus` silently accepted any value (and then
errored on the file-read path or on daemon dispatch), drifting
within the same namespace from `caco audio prewarm --model bogus`
which validates upfront against the SUPPORTED_STT_MODELS set.
Match the prewarm validator on transcribe so both audio surfaces
reject unsupported models cleanly.

## Bead(s)

- `bd-241b84` — caco audio sweep (P4 bug, multi-issue). Pins
  Issue 3. Issues 1-2 are POSITIVES (NEW gold-standard EITHER-OR
  required-flag pattern; 6th inline-allowed-values phrasing
  variant — converge candidates for the pattern catalogue). Issue
  4 (transcribe --json BROKEN exit 2, 3rd surface in --json-
  ignored-on-error family) is the same anti-pattern as bd-87425e
  image generate / bd-f4957c node show — needs same wrapper-at-
  dispatch-boundary treatment as bd-87425e. Issue 5 (12th empty-
  string-bypass) covered by bd-29c7e3 cross-cutting helper meta-
  bead. Issue 6 (default-aware prewarm self-documenting message)
  is positive observation.

## Before state

```
$ caco audio prewarm --model bogus
error: Failed to prewarm model 'bogus': Unsupported STT model 'bogus'. Supported: gpt-4o-mini-transcribe, whisper, scribble

$ caco audio transcribe --file /tmp/bogus.wav --model bogus
error: failed to read audio file '/tmp/bogus.wav': No such file or directory
                                                # --model never validated
```

If a real file existed, the bogus model would silently reach the
daemon, where it might be silently degraded to a default (or
return an opaque server-side error).

## After state

```
$ caco audio transcribe --file /tmp/bogus.wav --model bogus
error: unsupported STT model 'bogus'. Supported: gpt-4o-mini-transcribe, whisper, scribble
                                                # validates BEFORE file read
```

Sister-symmetric with `caco audio prewarm --model bogus`. The
transcribe validator fires upfront so it doesn't matter whether
the file exists or whether the daemon is up.

## Diff summary

- 1 file changed, +14 / -1 (`crates/caco-cli/src/lib.rs`):
  - `dispatch_audio_transcribe` now validates `model` against
    `SUPPORTED_STT_MODELS` before reading the audio file.

## Validation

- `cargo check -p caco-cli`: clean.

## Operator-takeaway

`caco audio transcribe --model X` now rejects unsupported model
values upfront with the same `Supported:` listing used by
prewarm. Operator scripting hazard removed; sister-surface drift
fixed within the audio namespace.

The shared SUPPORTED_STT_MODELS list now appears in two places
(prewarm via daemon-side check + transcribe via this client-side
upfront check). Future drift-prevention move: hoist into a single
`stt_models()` helper in caco-config — left for a follow-up.

Push-discipline (post-clarification): own-branch push allowed;
default-branch force-push banned; only reintegrate / complete
land work on main. This session continues to use only local refs
+ daemon-mediated reintegrate.
