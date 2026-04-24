# Session summary — bd-d10e9f: fix agent artefacts help and JSON contract

## Goal

Polish the new `caco agent artefacts` CLI surface so its help text matches the
actual ambient-agent fallback behavior, its JSON mode is always machine-
readable, and its help points operators toward the richer TUI/web browsing path
instead of implying a nonexistent `--project` filter.

## Bead(s)

- `bd-d10e9f` — `caco agent artefacts` help/JSON/doc-gap follow-up

## Before state

- `--help` marked `--id` as required even though the command already fell back
  to ambient `CACO_AGENT_ID` / `CACOPHONY_AGENT`.
- The help text did not explain that fallback behavior.
- `--json` emitted a raw payload shape and the empty-result path was vulnerable
  to returning non-canonical text instead of a stable JSON envelope.
- The command surface did not explain that project/workspace-aware artefact
  browsing lives in TUI/web agent-detail views rather than a CLI `--project`
  flag.

## After state

- `--id` is now documented as optional in help metadata.
- Help text explicitly says the command defaults to ambient agent env vars when
  `--id` is omitted.
- `caco agent artefacts --json` now returns a canonical `{ok,data,meta}`
  envelope.
- The command summary and `--id` help text now explicitly redirect operators to
  the TUI/web artefacts view for project/workspace-aware browsing.
- Added a source-contract test pinning both the help metadata and JSON shape.

## Diff summary

- Files touched:
  - `crates/caco-cli/src/lib.rs`
- Tests:
  - `cargo test -p caco-cli agent_artefacts_help_and_json_contract_bd_d10e9f -- --nocapture`
  - `cargo run -q -p caco -- agent artefacts --help`
  - `cargo run -q -p caco -- agent artefacts --json`
- Behavioural delta:
  - The artefacts CLI now tells the truth about ambient fallback and behaves
    like a machine-readable JSON surface should.

## Operator-takeaway

This was a classic fresh-surface UX cleanup: the command behavior itself was
mostly fine, but the help metadata and JSON contract had drifted enough to
mislead both humans and scripts. That mismatch is now closed.
