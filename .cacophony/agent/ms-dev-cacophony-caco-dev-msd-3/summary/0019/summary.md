# Session summary — bd-7a8bc1 slice 1: SttErrorToast state + indicator render

## Goal

Land AC#1 + AC#2 + AC#5 of bd-7a8bc1 (parent bd-c16753 split-off) so
the STT error-toast surface is wired up at the state and render
layers, ready for follow-on slices to wire the daemon error stream
(AC#4) and the open-doctor click routing (AC#3).

## Bead(s)

- `bd-7a8bc1` — [stt-ux] AC4: STT error toast (slice 1 of 5
  acceptance criteria)
- Drive-by: relayed compile-break fix from ms-mac-cacophony-caco-tui
  (missing `peer_consult_timeout_ms` field in
  `crates/caco-sidecar/src/lifecycle.rs:3491`)

## Before state

- `SpeechState` had no error-toast field. STT errors (engine fail,
  mic perm denied, transcribe HTTP 5xx, model missing) were
  invisible to the operator unless they happened to be tailing
  daemon.log.
- `cargo check --workspace --tests` was failing on
  `crates/caco-sidecar/src/lifecycle.rs:3491` with
  "missing field `peer_consult_timeout_ms`" — recent
  `TopLevelBeadsConfig` field addition not propagated to one
  test-only initializer.

## After state

- New `SttErrorToast { class, first_line, occurred_at }` type in
  `speech.rs` with:
  - `TOAST_TTL = 6s` (per AC#2).
  - `new()` truncates message at first `\n` and trims whitespace so
    the toast is guaranteed single-line.
  - `is_expired()` with inclusive boundary at TOAST_TTL.
- `SpeechState.stt_error_toast: Option<SttErrorToast>` field
  (initialised to `None`).
- `stt_error_toast_spans(speech, now)` in `speech_indicator.rs`
  renders `⚠ [<class>] <first_line>  open doctor` with NORD11 red
  for the icon+class and underlined NORD8 cyan for the link label.
  Returns empty `Vec` when toast is `None` OR expired so the steady-
  state render path is a no-op.
- `crates/caco-sidecar/src/lifecycle.rs:3491` patched with
  `peer_consult_timeout_ms: None` to match the 10+ other call
  sites; workspace `--tests` now compiles clean.

## Diff summary

- `crates/caco-tui/src/speech.rs`: +SttErrorToast type (66 lines),
  +stt_error_toast field, +3 tests.
- `crates/caco-tui/src/views/speech_indicator.rs`:
  +stt_error_toast_spans() helper, +3 tests.
- `crates/caco-sidecar/src/lifecycle.rs`: +1 line
  (`peer_consult_timeout_ms: None`) — drive-by compile fix.
- 232 insertions across 2 files (test mod growth dominates).
- `cargo test -p caco-tui --lib stt_error_toast`: 6/6 pass.
- `cargo check --workspace --tests`: clean.

## Embedded artefacts

(none)

## Operator-takeaway

Slice 1 lands the surface. Two remaining acceptance criteria are
out-of-tree from a single agent's incremental claim:
- **AC#3** (open-doctor link routing): touches the nav module
  which is currently being modified by other agents (Merge Queue
  reorder just landed, Timeline view in flight). Best left to a
  nav-touching agent to bundle.
- **AC#4** (daemon STT error stream → toast): cross-cuts
  `crates/caco-daemon/src/scribble_stt.rs` error paths and the
  TUI driver loop's tick handler. Probably wants its own bead
  slice for the stream-protocol design (currently STT errors are
  logged-and-swallowed, not surfaced via a typed event).

Bead unclaimed for follow-on work.
