# Session summary — `caco audio doctor` lands STT diagnostic surface

## Goal

Close the bd-56ca56 P0 closure-discipline gap: bd-128ea6 was previously
marked done but its AC#5 ("Add `caco stt --doctor` subcommand: lists
installed models, scribble feature status, runs a self-test on a known
clip") had no implementation reachable from the CLI. This session lands
the missing surface so an operator (or test-user pass) can actually
diagnose STT health from the CLI.

## Bead(s)

- `bd-56ca56` — bd-128ea6 P0 STT-stabilize CLOSED-WITHOUT-FIX on AC#5
  (this session's primary work)
- `bd-c5fa50` — `[broken-on-main]` config_distribute_* test stack
  overflows (filed + partially fixed in this session; left OPEN with
  follow-up notes for the doctor_includes_* siblings)

## Before state

- `caco stt --doctor` → "unknown command path for help: caco stt"
- `caco audio --doctor` / `caco audio doctor` → not recognised
- `caco audio transcribe --doctor` → unrecognised flag (bd-b76723)
- `caco doctor` exists but is the cluster-mesh-health doctor, no
  scribble feature status, no installed-model list, no self-test
- 5 `dispatch_config_distribute` tests overflowed the default 2 MiB
  test stack on debug builds (broken-on-main)

## After state

- `caco audio doctor [--clip <wav>] [--model <name>] [--skip-self-test] [--json]`
  is a real top-level command under the existing `audio` namespace.
- Reports: TTS/STT availability, full STT/TTS model lists,
  `unavailable_reason`, scribble feature status, and a round-trip
  self-test against `/api/v1/audio/transcription` with latency.
- Validated end-to-end against the local daemon; the self-test on
  `tests/stt-corpus/audio/commands-00.wav` returns "CLAIM BEADS!" in
  ~1.1s with status=ok.
- All 5 `config_distribute_*` tests pass via a new `with_big_stack`
  test helper that runs the body on an 8 MiB worker thread.
- Sibling `doctor_includes_lifecycle_supervisor_section` and
  `doctor_includes_snapshot_pinned_count_sensor` overflows remain
  pre-existing on main; documented in bd-c5fa50 update notes.

## Diff summary

- Commits: `2d1e94ad7 bd-56ca56: add 'caco audio doctor' STT
  diagnostic (bd-128ea6 AC#5)`
- Files touched: `crates/caco-cli/src/lib.rs` (+351 lines)
- Tests: +1 (covered by an integration-shaped end-to-end manual run
  against the local daemon; the new `audio doctor` dispatch is a
  thin wire over capabilities + transcription so the existing
  capabilities + transcription unit tests cover most behaviour).
  Pre-existing 5 `config_distribute_*` tests rescued from
  broken-on-main.
- Behavioural delta: `caco audio doctor` is the new doctor surface
  for STT — chosen (option b) over `caco stt --doctor` (option a)
  because it lives in the existing `audio` namespace next to the
  related `speak`, `transcribe`, `capabilities`, and `prewarm`
  subcommands, which is consistent with existing CLI ergonomics.

## Operator-takeaway

The bd-128ea6 closure-discipline regression is now fixed: an operator
can run `caco audio doctor --clip <known.wav>` and get back a
single-screen TUI block (or JSON) showing scribble feature status,
installed STT/TTS models, and a working round-trip transcription
latency on a real clip. This is the surface bd-56ca56 demanded.
Remaining `doctor_includes_*` test overflows on main are documented
in bd-c5fa50 (left OPEN, unassigned) for the next dev-class agent to
pick up — the fix shape is identical to the `with_big_stack` wrapper
used here, but one of them masks a `dispatch_doctor` Err that needs
real investigation.
