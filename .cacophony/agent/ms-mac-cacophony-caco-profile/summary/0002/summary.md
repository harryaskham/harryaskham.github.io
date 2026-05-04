# Session summary — fresh Pi launch prompt preservation

## Goal

Fix `bd-1e2f4a`, where a freshly launched managed Pi worker could be revived via `pi --continue` even though no concrete Pi session file existed, leaving the live pane at a generic/blank Pi prompt without the assigned bead goal or managed system prompt.

## Bead(s)

- `bd-1e2f4a` — `[managed-agent] Fresh Pi worker launched via pi --continue into empty session`

## Before state

- Fresh managed Pi `init.sh` unconditionally included `--continue`.
- The resume path fell back to `pi --continue` when `harvest_pi_session_path` found no `.pi-agent/sessions/*.jsonl` file.
- Evidence in the bead showed `system-prompt.txt` and `prompt.txt` existed but the relaunched pane only showed generic Pi UI, not the assigned bead goal/lifecycle instructions.

## After state

- Fresh managed Pi launch no longer injects `--continue`.
- Pi resume still uses `--session <path>` when a real Pi session file is available.
- If no concrete Pi session exists, resume/relaunch uses a new `PiFreshLaunch` path that strips stale `--continue` / `--session`, reattaches `system-prompt.txt` via `--append-system-prompt`, reattaches `prompt.txt` as the goal prompt, and keeps the resume watchdog disabled in the rewritten `init.sh`.
- Resume metadata now exposes `pi_fresh_launch` distinctly from `pi_session_resume` and `pi_continue`.

## Diff summary

- Commit: `bfd9920d7 bd-1e2f4a: preserve prompts on fresh Pi launch`
- Files touched:
  - `crates/caco-daemon/src/agent/spawn.rs`
  - `crates/caco-daemon/src/agent/lifecycle.rs`
  - `crates/caco-daemon/src/agent/types.rs`
  - `crates/caco-daemon/src/agent/tests.rs`
- Validation via first-party queue surfaces:
  - `CARGO_BUILD_JOBS=2 cargo test -p caco-daemon pi_fresh --lib -- --test-threads=2` — passed (`tj-12946461`)
  - `CARGO_BUILD_JOBS=2 cargo test -p caco-daemon init_script_pi_does_not_include_continue_on_fresh_launch --lib -- --test-threads=2` — passed (`tj-013f09d8`)
  - `CARGO_BUILD_JOBS=2 cargo test -p caco-daemon resume_method_ --lib -- --test-threads=2` — passed (`tj-d3e2bb53`)
  - `CARGO_BUILD_JOBS=2 cargo test -p caco-daemon build_resume_init_script_pi_without_session --lib -- --test-threads=2` — passed (`tj-e992754c`)
  - `rustfmt --edition 2021 --check crates/caco-daemon/src/agent/lifecycle.rs crates/caco-daemon/src/agent/spawn.rs crates/caco-daemon/src/agent/tests.rs crates/caco-daemon/src/agent/types.rs` — passed (`tj-9c690e7d`)
- Note: full `cargo fmt --all -- --check` through the queue reported an unrelated pre-existing `crates/caco-daemon/src/audio.rs` formatting diff. That file was not included in this bead commit.

## Operator-takeaway

A managed Pi worker without prior Pi session state should now start with the resolved Cacophony system prompt and assigned bead goal rather than entering a blank/default `pi --continue` session.
