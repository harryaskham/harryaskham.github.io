# Session summary — verify create-and-claim ownership persistence

## Goal

Make `caco bd create --claim true` safe for worker ownership workflows by ensuring the CLI no longer reports `created and claimed` solely from the immediate claim response when the authoritative bead state has not actually persisted the assignment.

## Bead(s)

- `bd-9d60d9` — Ensure `caco bd create --claim` persists ownership before reporting success.

## Before state

- Failing tests: none attributable to this bead at start.
- Relevant metrics: existing `bd create --claim` focused tests covered successful claim, partial claim failure, and empty claim payload, but not a stale/mismatched post-claim board read.
- Context: the CLI created a bead, issued a follow-up claim, and printed success if the claim response had canonical fields; a later `bd show/list` could still show `open` and unassigned, creating duplicate-worker risk.

## After state

- Failing tests: `cargo test-small` currently fails on pre-existing `caco-tui app::tests::ctrl_r_persists_state_for_restart`; tracked by `bd-da6124` and owned by `cacophony:ms-mac-cacophony-caco-tui`.
- Relevant metrics: focused create-with-claim tests pass: 4 passed, 0 failed. `cargo clippy -p caco-cli --all-targets -- -D warnings`, `docs/validate-pages.sh`, and `git diff --check` pass.
- Context: after a successful claim response, `bd create --claim` now performs an authoritative bead detail read and only reports success if the persisted bead has matching id, assignee, and claimed status.

## Diff summary

- Commits: `035a7e35a`.
- Files touched: `crates/caco-cli/src/lib.rs`, `SPEC.md`, `README.md`, `AGENTS.md`, `docs/beads.html`, `docs/cli.html`.
- Tests: +1 regression for post-claim persistence verification; existing create-with-claim tests updated for the new verification GET.
- Behavioural delta: mismatched or unverified post-claim state now returns non-zero `claim_after_create_unverified` with create/claim/read evidence in JSON mode instead of misleadingly printing `created and claimed`.

## Operator-takeaway

Workers can trust `caco bd create --claim true` success output more strictly now: success requires persisted ownership, while ambiguous claim persistence fails loudly and tells the worker to inspect the bead before editing or retrying.
