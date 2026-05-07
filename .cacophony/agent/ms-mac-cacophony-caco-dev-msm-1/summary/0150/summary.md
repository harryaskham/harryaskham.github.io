# Session summary — machine-readable loop claim audits

## Goal

Complete `bd-1d7ded` by preventing managed loop and self-audit guidance from depending on greps over the coloured human `caco bd list` table, and make `caco bd list --count-only` reliable when the CLI must apply current-agent assignee alias filtering client-side.

## Bead(s)

- `bd-1d7ded` — Make managed loop claim audits use machine-readable bead output.

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: `caco bd list --assignee $CACO_AGENT_ID --status in_progress --count-only` could return the daemon-side count before current-agent alias post-filtering, and profile self-audit snippets included an `awk '/^bd-/'` pattern over human table output.
- Context: the bead was filed after a managed self-check loop missed active claims because ANSI colour prefixes prevented a line-start grep/awk from seeing bead IDs.

## After state

- Failing tests: none observed in targeted validation.
- Relevant metrics: queued `tj-06230f04` passed the new caco-cli unit test; queued `tj-e77c389f` ran this checkout's `cargo run -p caco -- bd list ... --count-only` and returned `1` for this agent's active claim instead of the installed old CLI's unfiltered count.
- Context: `--count-only` now fetches bounded machine-readable rows when client-side filters are required, applies the same filtering path as normal list output, and formats the filtered `data.count` locally.

## Diff summary

- Code/content commits: `a23ede552f`.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched: `crates/caco-cli/src/lib.rs`, `.cacophony/profiles/dev.md`, `.cacophony/profiles/auto-claim.md`, `AGENTS.md`.
- Tests: +1 caco-cli unit test; no tests removed or flipped.
- Behavioural delta: managed loop claim audits now have documented JSON/count-only patterns, and count-only current-agent checks use filtered counts rather than raw daemon counts when alias post-filtering is in play.

## Operator-takeaway

The specific footgun that caused duplicate claims is now fixed at both layers: prompts no longer recommend parsing coloured human output, and the preferred `--count-only` gate now respects the same current-agent alias filtering as regular list output.
