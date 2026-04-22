# Session summary — bd-30fbfb: bd search --query primary, --test-name deprecated

## Goal

Stop forcing `caco bd search` users to spell `--test-name`
when the subcommand is a generic substring search across
bead title/description/spoken-name in all statuses.

## Bead(s)

- `bd-30fbfb` — own UX papercut filed after hitting the
  friction directly. Closed.

## Before state

- `caco bd search` required `--test-name=<substring>`.
  Name leaked from a long-gone test-finding feature.
- Repro: `caco bd search --query 'node show'` →
  `error: --test-name is required for bd search`.
- Misleading because nothing about the subcommand
  involves tests.

## After state

- `--query` added as the preferred flag name; help text
  leads with the substring-search description.
- `--test-name` kept as an explicit deprecated alias
  with summary text marking it as such (back-compat for
  anyone scripting against the old flag).
- Either flag accepted; if both supplied, `--query` wins.
- New error text: "--query is required for bd search
  (--test-name accepted as deprecated alias)".

## Diff summary

- 1 file touched, +20 / −5:
  - `crates/caco-cli/src/lib.rs`: added `--query` to
    `BD_SEARCH_ARGS`; updated `dispatch_bd_search` to
    prefer `--query` then fall back to `--test-name`.

## Verification

- `cargo build -p caco-cli`: clean.

## Operator-takeaway

Family with bd-2b10dd / bd-b45e48 / bd-513fc8 /
bd-eb84c8 / bd-126b99 / bd-a403a1 (CLI honesty pass) —
flag names should describe what they do, not historical
implementation accidents. `--test-name` retained for
back-compat, no caller forced to migrate.
