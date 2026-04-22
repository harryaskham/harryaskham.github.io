# Session summary — spoken-name stop-word expansion (bd-2d7e62)

## Goal

`generate_spoken_name()` in `crates/caco-beads/src/model.rs` produced awkward TTS output like `And-Deploy` or `For-Agent` because its stop-word list was biased toward 2-letter words. Expand to cover common 3-letter filler words.

## Bead(s)

- `bd-2d7e62` — Expand spoken name stop-word list to include common 3-letter words. Promoted from draft → open before claiming.

## Before state

Stop list: `["a","an","at","by","do","if","in","is","it","no","of","on","or","so","to","up","we","the"]`. Titles starting with "And…", "For…", "Why…" generated spoken names beginning with that filler.

## After state

Added a curated 3-letter stop list: `and / are / but / can / did / for / had / has / her / him / his / its / may / nor / not / now / our / out / own / she / the / too / was / who / why / yet / you`. Domain nouns that happen to be three letters (`web / api / git / dns / ssl`) are intentionally not stopped — operators want to hear them in spoken names.

- `cargo test -p caco-beads --lib spoken_name` — 5 / 5 pass (including the new `spoken_name_filters_3_letter_fillers`).
- `cargo test-small` — clean across the workspace (198 / 109 / 720 / 291 / 18 / 2813 / 52, 0 failed).
- `cargo check --workspace --tests` — clean.

## Diff summary

- Commit: `ee3037ba`
- Files touched: `crates/caco-beads/src/model.rs` (stop list + 1 new test).
- Tests: +1 unit; 0 removed; 0 flipped.
- Behavioural delta: net better spoken names for affected titles; no API change. Existing beads keep their stored `spoken_name`; only newly-created beads (or explicit re-derive paths) pick up the new behaviour.

## Out of scope

- Backfill existing beads' `spoken_name` to use the new list — operator decision (some beads may have shipped audio cues that match the old name).
- Locale-aware stop lists.

## Operator-takeaway

Auto-generated spoken names will sound noticeably more natural for titles that lead with common filler words. No config change, no migration, no operator action required.
