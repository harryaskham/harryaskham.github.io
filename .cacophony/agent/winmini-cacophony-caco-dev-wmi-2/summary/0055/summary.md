# bd-b1320e post-merge gate validation

## Bead
- bd-b1320e — Merge-queue/reintegration compile gate must validate the POST-MERGE result, not the agent branch in isolation.

## Changes
- Updated `plugins/caco-agent/agents/fast-test-gate.sh` to prepare a temporary post-merge validation worktree at `origin/<target>` and squash-merge the current agent `HEAD` before running the configured test/check/clippy gates.
- The gate now runs queue-backed or inline commands from that post-merge worktree, so an agent branch that compiles in isolation but breaks after combining with current main is rejected before publish.
- Added a final target-tip check that blocks if `origin/<target>` advances while the post-merge gate is running.
- Added profile regression coverage asserting the fast-test-gate script contains the post-merge worktree/merge/freshness guard pieces.
- Updated SPEC, README, and AGENTS to document post-merge gate semantics.

## Validation
- `bash -n plugins/caco-agent/agents/fast-test-gate.sh`
- `cargo test -p caco-profile fast_test_gate_script_validates_post_merge_tree_bd_b1320e --lib -- --test-threads=1`
- `cargo check -p caco-profile --lib`
- Manual local fixture: created a temporary git repo/remote, ran `fast-test-gate.sh` with `CACO_REINTEGRATION_GATE_QUEUE=0` and `CACO_REINTEGRATION_CHECK_CMD='test -f feature.txt'`; the check passed only from the post-merge validation worktree.
