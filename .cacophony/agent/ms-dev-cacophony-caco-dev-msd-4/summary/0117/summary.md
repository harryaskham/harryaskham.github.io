# Session summary — Summaries next-index help and stale-runtime fallback

## Goal

Resolve `bd-3186f3`, where live stale `caco` binaries rejected `caco summaries next-index --agent ...` and `caco help summaries next-index` even though current source already had metadata for the helper.

## Bead(s)

- `bd-3186f3` — `Align summaries next-index runtime help and --agent flag behavior`

## Before state

- Failing tests: none in current source, but live `caco 1.2.565` reproduced the issue: `summaries next-index --agent ... --no-fetch` treated `--agent`/`--no-fetch` as unknown flags on `summaries`, and nested help returned `unknown command path`.
- Relevant metrics: Current source `cargo run -p caco -- summaries next-index --agent ... --no-fetch` succeeds, and `cargo run -p caco -- help summaries next-index` shows `--agent`, `--state-branch`, and `--no-fetch`.
- Context: The root cause in this session was runtime skew, but the guidance did not tell agents what to do when a live node is stale.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: Added CLI regression tests that fail if current source ever regresses to treating `--agent` / `--no-fetch` as unknown parent-command flags or loses nested help for `summaries next-index`.
- Context: Session-recording guidance, README, AGENTS, and SPEC now explicitly say to treat those failures as stale runtime skew and to fall back to `summaries list --agent ... --json` plus local summary directories while tracking the runtime update separately.

## Diff summary

- Commits: implementation commit `bd-3186f3: pin summaries next-index help and fallback guidance` plus this summary commit.
- Files touched: `crates/caco-cli/src/lib.rs`, `.cacophony/profiles/session-recording.md`, `README.md`, `SPEC.md`, `AGENTS.md`.
- Tests: added `summaries_next_index_accepts_agent_flag_bd_3186f3` and `summaries_next_index_help_resolves_bd_3186f3`.
- Behavioural delta: current-source behavior is pinned; stale live-runtime failure mode is documented with a safe fallback instead of blocking summary authoring.
- Validation: `cargo fmt --all -- --check`; `cargo test -p caco-cli bd_3186f3 -- --nocapture`.

## Operator-takeaway

The helper contract is now tested, and agents have explicit instructions for the exact stale-runtime symptom seen on old 1.2.565 nodes: use the list/local fallback and track the node update instead of getting stuck.
