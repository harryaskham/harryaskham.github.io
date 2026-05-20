# Session summary — attach by persistent @ name

## Goal

Implement `bd-deee1d` by allowing `caco agent attach @caco-ctrl`-style targets to resolve persistent controller names with the same safe matcher used for lifecycle nudge-by-name flows.

## Bead(s)

- `bd-deee1d` — Implement @ syntax for agent attach to match controller names

## Before state

- Failing tests: none known for this bead at claim time.
- Relevant metrics: `caco agent attach` required `--id` or environment agent id; a positional `@caco-ctrl` could not be used to attach to a persistent controller by its operator-facing name.
- Context: the repository already had a persistent-agent name resolver for lifecycle nudge paths, including exact, suffix, substring, and ambiguity handling.

## After state

- Failing tests: none in focused validation.
- Relevant metrics: passed `RUST_MIN_STACK=33554432 cargo test -p caco-cli --lib bd_deee1d -- --test-threads=1`, `RUST_MIN_STACK=33554432 cargo test -p caco-cli --lib agent_attach_command_spec_has_id_and_raw_args -- --test-threads=1`, `cargo clippy -p caco-cli --lib -- -D warnings`, `cargo run -p caco -- agent attach @caco-dev-po4-3 --json`, and `git diff --check`. `./scripts/rustfmt-changed.sh` still skips `crates/caco-cli/src/lib.rs` due known pre-existing rustfmt drift.
- Context: `--id` remains available as the explicit stable-id path, but it is no longer marked required in CLI metadata because a positional `@<persistent-name>` target is now accepted.

## Diff summary

- Code/content commits: `e78c50ae8`.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched: `crates/caco-cli/src/lib.rs`, `SPEC.md`, `README.md`.
- Tests: +1 pure normalization test for `@caco-ctrl` / `@agent:caco-ctrl`; updated the attach command metadata test to pin optional `--id` with positional `@` support.
- Behavioural delta: `caco agent attach @<persistent-name>` now strips the `@`/optional `agent:` prefix, resolves through persistent-name exact/suffix/substring matching, and then uses the existing attach transport path. Ambiguous names still fail closed with existing candidate guidance.

## Operator-takeaway

Controller-style attach is now ergonomic without weakening safety: operators can type `caco agent attach @caco-ctrl`, while ambiguous substrings still require choosing or spelling the exact target instead of silently attaching to the wrong agent.
