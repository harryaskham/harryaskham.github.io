# Session summary — feed JSONL doctor repair

## Goal

Add a first-party, operator-safe way to diagnose and repair corrupt daemon `feed.jsonl` records after the ms-mac outage required ad hoc invalid UTF-8 recovery. The goal was to make future recovery inspectable, bounded, and explicitly confirmed rather than silent or destructive.

## Bead(s)

- `bd-72ffc4` — Add first-party feed JSONL doctor/repair for invalid UTF-8 and malformed records
- Reflection filed: `bd-659636` — Ensure macOS devshell exposes libiconv library path for cargo checks

## Before state

- Failing tests: none known for this bead at start.
- Relevant metrics: no `caco doctor feed` command existed; operators had to use ad hoc scripts to identify invalid UTF-8/malformed JSONL records.
- Context: the ms-mac outage involved a corrupt `~/.cacophony/daemon/feed.jsonl` line. Existing daemon-side hardening addressed startup tolerance, but there was no CLI repair surface with backup/quarantine and explicit confirmation.

## After state

- Failing tests: none in the focused validation run.
- Relevant metrics: added two focused `caco-cli` unit tests covering invalid UTF-8/malformed JSON scan reporting and explicit-confirmation repair/quarantine.
- Context: `caco doctor feed` now scans the configured daemon feed (or `--path`) and `caco doctor feed --repair --yes` backs up the feed, quarantines corrupt records, rewrites only valid records, and reports backup/quarantine paths.

## Diff summary

- Commits: code commit `324357764`; summary is committed in this session-summary commit.
- Files touched: `crates/caco-cli/src/lib.rs`, this summary file.
- Tests: +2 focused unit tests.
- Behavioural delta: added `doctor feed` help/dispatch surface, text/JSON scan output, invalid UTF-8 and malformed JSONL detection with line/byte details, and a write path gated by explicit `--repair --yes` that preserves original data in timestamped backup/quarantine files.
- Validation: `cargo fmt --package caco-cli`; `LIBRARY_PATH=/nix/store/a85h00app701vf0ggln0r97yayszvwkk-libiconv-109.100.2/lib RUST_MIN_STACK=33554432 cargo test -p caco-cli --lib doctor_feed -- --nocapture`; `LIBRARY_PATH=/nix/store/a85h00app701vf0ggln0r97yayszvwkk-libiconv-109.100.2/lib cargo check -p caco-cli --lib`; `git diff --check`.

## Operator-takeaway

Future feed corruption recovery no longer depends on one-off SSH/Python repair: operators can run a first-party doctor scan, review precise corrupt-record findings, and only then opt into an explicit backup-and-quarantine repair path that avoids silent data loss.
