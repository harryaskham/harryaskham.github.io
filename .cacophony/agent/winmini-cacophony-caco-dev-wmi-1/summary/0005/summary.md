# Session summary — broken-on-main fix + bd-c16753 STT-UX split

## Goal

Two interleaved deliverables in one session:

1. **bd-54b3ee** — fix a broken-on-main `cargo check --workspace --tests`
   failure surfaced by ms-mac-cacophony-caco-tui while validating
   bd-b544e8: a single missing `peer_consult_timeout_ms` initializer
   in a sidecar lifecycle test.
2. **bd-c16753** — STT visual-indicators umbrella: surveyed existing
   implementation (AC1 already done), then split the remaining four
   acceptance criteria into landable sub-beads so other workers can
   pick them up in parallel rather than this P0 blocking on a single
   monolithic claim.

## Bead(s)

- `bd-54b3ee` — broken-on-main: caco-sidecar lifecycle test missing
  `peer_consult_timeout_ms` initializer (created + claimed + landed).
- `bd-c16753` — STT visual indicators umbrella (claimed, surveyed,
  released back to open with split children documented).
- `bd-7a8bc1` — child: AC4 STT error toast with open-doctor link (P1).
- `bd-88798a` — child: AC2 partial ghost text + AC3 final-commit flash
  (P2).
- `bd-db10da` — child: AC5 speech-popup hide toggle (P3).

## Before state

- `cargo check --workspace --tests` failed at
  `crates/caco-sidecar/src/lifecycle.rs:3491` with E0063 missing field
  `peer_consult_timeout_ms` in `TopLevelBeadsConfig` initializer.
  Every other call site (caco-config `validate.rs` ~10 occurrences,
  caco-daemon `beads.rs` / `election.rs` / `multinode.rs`) was already
  complete; this single sidecar test was missed when the field was
  added.
- bd-c16753 was an open P0 with six acceptance criteria spanning the
  TUI tab bar, speech-popup view, state machine, toasts, audio
  chime, and tests — too large to land cleanly in one session.

## After state

- `cargo check --workspace --tests` is clean (verified locally before
  commit).
- bd-c16753 description is rewritten to record the survey:
  - AC1 (the persistent red/grey/green/amber dot) is already
    implemented at `crates/caco-tui/src/speech.rs:403-437`,
    `crates/caco-tui/src/views/speech_indicator.rs:88-98`, and the
    tab bar wires it at `crates/caco-tui/src/views/tab_bar.rs:106`.
  - AC2/AC3/AC4/AC5 are not implemented and now have dedicated
    sub-beads.
- Three sub-beads created with explicit acceptance criteria, file
  hints, and parent linkage so other workers can claim independently.
- Released the umbrella back to `open` so it stays as the parent
  tracking surface; it can close when all three children land.

## Diff summary

- 1 file changed, +1 / -0:
  - `crates/caco-sidecar/src/lifecycle.rs:3491` — add
    `peer_consult_timeout_ms: None` to the
    `lifecycle_manager_discovers_standalone_bd_daemon_service`
    test's `TopLevelBeadsConfig` initializer.

## Validation

- `cargo check --workspace --tests --message-format=short`: clean
  (was 1 error before).

## Operator-takeaway

The compile fix is a one-liner; the more useful artefact here is the
bd-c16753 split. Splitting an umbrella P0 into a P1 + P2 + P3 trio
unblocks parallel pickup without losing the original scope — the
umbrella stays as the tracking surface and closes when the children
land. This is the right shape for any acceptance-criteria-list bead
that's larger than a single session: survey first, document the
already-done parts, then file the remaining parts as their own beads.
A follow-up worth filing later (P3): a `caco bd split` CLI surface
that automates this pattern (parse "## Acceptance criteria" list,
emit one child per item, link parent, copy provenance).
