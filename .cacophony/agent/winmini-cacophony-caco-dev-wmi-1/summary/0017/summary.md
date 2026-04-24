# Session summary — bd-9eb9f8 codespace ls status listing

## Goal

Burn down the next concrete child bead from the Codespaces lifecycle breakdown by implementing `caco codespace ls`, so operators can list live GitHub Codespaces and see their derived Cacophony node ids without dropping straight to raw `gh` commands.

## Bead(s)

- `bd-9eb9f8` — Implement `caco codespace ls` status listing
- (parent: `bd-869dca` — Add Codespaces node lifecycle management)

## Before state

- Failing tests: none in scope before this change.
- Relevant metrics: the repo already had `caco codespace new` and `caco codespace enroll`, and `docs/codespaces.md` already documented a `caco codespace ls` surface, but the CLI family only registered the two creation/enrollment commands.
- Context: operators had docs and examples for `caco codespace ls --json`, including jq snippets that expected a codespace hash, but there was no implemented list/status command behind that contract.

## After state

- Failing tests: none observed in the focused `caco-cli` and docs/bootstrap validation set.
- Relevant metrics: `caco codespace ls` now exists, supports `--limit`, `--repo`, and `--refresh`, shells out to `gh codespace list --json ...`, derives stable `cs-<hash>` node ids from codespace names, and emits a canonical `{ok,data,meta}` JSON envelope with `data.codespaces[]`.
- Context: the docs/examples now match the implemented command surface, and a live dry run under my current GitHub auth fails cleanly with a user-facing `gh codespace list failed: ...` error rather than panicking.

## Diff summary

- Commits: `c7a9143e2`
- Files touched: `crates/caco-cli/src/lib.rs`, `docs/codespaces.md`, `README.md`, `AGENTS.md`
- Tests: `cargo test -p caco-cli caco_codespace_subcommands_are_registered -- --nocapture`; `cargo test -p caco-cli parse_codespace_list_entries_derives_node_ids_bd_9eb9f8 -- --nocapture`; `cargo test -p caco-cli format_codespace_list_json_envelope_bd_9eb9f8 -- --nocapture`; `cargo test -p caco-cli dispatch_codespace_new_pushes_rendezvous_bootstrap_secret_bd_0bed93 -- --nocapture`; `cargo run -q -p caco -- codespace ls --help`; `./target/debug/caco codespace ls`; `python -m json.tool .devcontainer/devcontainer.json`; `bash -n .devcontainer/cacophony-bootstrap.sh`
- Behavioural delta: operators now have a first-party `caco codespace ls` status command with live GitHub-backed state, derived node ids/hashes, human-readable output, and a stable machine-readable JSON contract.

## Operator-takeaway

This closes the most obvious gap in the new Codespaces lifecycle breakdown: the docs no longer promise a status-listing surface that doesn’t exist, and future lifecycle slices can now build on a real first-party `ls` command instead of ad hoc `gh` usage.
