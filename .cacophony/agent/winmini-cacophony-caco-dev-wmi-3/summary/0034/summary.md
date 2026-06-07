# Session summary — Passive STT tail CLI

## Goal

Implement `caco stt tail` as an ergonomic, passive transcript tail/follow surface for headless STT daemon transcript JSONL files, including `-f` follow, `-n` backlog count, and `--stt-daemon` filtering, while preserving the operator constraint that tailing must never advance or mutate the cursor used by `caco stt diff` consumers.

## Bead(s)

- `bd-2d56a2` — caco stt tail: continuous live STT line stream (-f follow, -n N last lines, --stt-daemon filter)

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: `caco stt diff --cursor --limit` existed for cursor-based consumers, and the STT daemon persisted retained transcript JSONL under `$CACOPHONY_DIR/stt-daemon/`, but there was no `tail`/`follow` CLI for operator-friendly passive viewing.
- Context: the operator added a constraint that `caco stt tail` must be observer-only and must not advance or consume the transcript cursor used by agents/listener profiles.

## After state

- Failing tests: none in the focused validation listed below.
- Relevant metrics: `caco stt tail [-f|--follow] [-n N] [--stt-daemon <id>]` is registered as a CLI command. It prints recent transcript lines, optionally follows new entries, merges all local STT daemon transcript files by timestamp when unfiltered, and restricts to one named instance when requested.
- Context: `tail` reads transcript JSONL directly with an independent in-memory seen set for follow mode; it never calls the daemon diff endpoint and never writes daemon cursor state.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched: `crates/caco-cli/src/lib.rs`, `crates/caco-cli/src/audio_cmd.rs`, `SPEC.md`, `README.md`, `AGENTS.md`, `docs/transcription.md`, `docs/transcription.html`.
- Tests: added focused tests for passive cursor preservation, multi-instance merging, JSON/filter output, and command registration/help safety.
- Behavioural delta: operators can now use `caco stt tail -n 10` for recent lines or `caco stt tail -f --stt-daemon <id>` for live passive follow without interfering with `caco stt diff` consumers.
- Validation run: `cargo test -p caco-cli stt_tail --lib`; `cargo test -p caco-cli caco_stt --lib`; `cargo check -p caco-cli --lib`; `cargo clippy -p caco-cli --lib -- -D warnings`; `docs/validate-pages.sh docs/transcription.md docs/transcription.html`; `./scripts/rustfmt-changed.sh --check crates/caco-cli/src/audio_cmd.rs`; `git diff --check`. `crates/caco-cli/src/lib.rs` has pre-existing rustfmt drift, so full-file rustfmt check still reports unrelated formatting churn; changed code was manually kept minimal and `git diff --check` is clean.

## Operator-takeaway

`caco stt tail` is deliberately a passive observer: it gives a convenient terminal stream of retained/live transcript lines but does not consume or advance the shared cursor that transcript-aware agents rely on for action policy.
