# Session summary — bd-cf528f unknown positional tokens

## Goal

Fix the parser silently dropping unknown positional tokens before
subcommand resolution, allowing typos like `caco worker status` to
silently dispatch to `caco status`.

## Bead(s)

- `bd-cf528f` — caco CLI dispatcher silently drops unknown POSITIONAL
  tokens before matching subcommand (parallel of bd-b76723 flags)

## Before state

- `caco worker status` → runs `caco status` silently, no warning.
- `caco bogus1 bogus2 status` → runs `caco status` silently.
- `caco xyzzy bd list --count-only` → runs `caco bd list` silently.
- CACO_STRICT_UNKNOWN_FLAGS=1 does NOT catch positional typos.

## After state

- Default: warn-then-proceed with bd-cf528f-styled stderr warning
  naming the unknown token(s) and the resolved command.
- CACO_STRICT_UNKNOWN_FLAGS=1: error (refuse to dispatch).
- Non-idempotent resolved commands: force-strict per bd-4c8fdd.
- ParsedCommand gains `unknown_prefix_tokens: Vec<String>`.
- parse_command_path tracks tokens pushed to positionals while the
  current path is an internal branch (has subcommands, not yet a leaf).
- dispatch calls warn_or_error_unknown_prefix_tokens.
- Legitimate positionals (`caco bd show bd-123`) are unaffected.

## Diff summary

- 1 file modified (`crates/caco-cli/src/lib.rs`), 102 insertions.
- ParsedCommand: +1 field (`unknown_prefix_tokens`).
- parse_command_path: +10 lines (branch check + push to tracker).
- New fn `warn_or_error_unknown_prefix_tokens` (~50 lines, mirrors
  `warn_or_error_unknown_flags_with_strictness`).
- dispatch: +15 lines (check + call after bd-b76723 block).
- cargo test-small: green.

## Operator-takeaway

Typo-safety for positional tokens now matches bd-b76723's flag-level
hygiene. `caco worker status` → clear warning. Strict mode catches
it. No false positives on legitimate positionals.
