# Summary 0022 — bd-020bc1: caco bd operator-actions

## Bead
bd-020bc1 (P2, feature, claimed) — operator-action dashboard:
formalize the [operator-action] subset so it doesn't depend on
operators grepping their queue for the title prefix.

## Change

### CLI

`crates/caco-cli/src/lib.rs`:

- New `bd operator-actions` subcommand registered in
  `BD_SUBCOMMANDS` with summary citing bd-020bc1.
- `BD_OPERATOR_ACTIONS_ARGS`: `--project`, `--max-age`,
  `--limit`, `--include-closed`.
- `dispatch_bd_operator_actions`: pulls `bd list?status=open` (or
  unfiltered when `--include-closed`), filters client-side to
  beads matching `is_operator_action_bead`, applies `--max-age`
  cutoff against `created_at`, returns through the standard
  `format_bead_list` path.
- `is_operator_action_bead(&serde_json::Value)`: extracted
  helper. Returns true iff the bead has `"operator-action"` in
  its labels array OR title starts with `"[operator-action]"`.
  Substring-match in the title does NOT count (only prefix).
- Reuses existing `parse_duration_secs` (bd-5e75ee) for
  `--max-age`.
- Response shape: writes the filtered array back into the
  `data.beads` slot of the daemon's bead-list envelope so the
  standard `format_bead_list` continues to work and JSON
  consumers get the same envelope they expect.

### Tests

`crates/caco-cli/src/lib.rs::tests::is_operator_action_bead_detects_label_and_title_prefix`:
- label match
- title-prefix match
- neither (negative case)
- substring-only-in-title doesn't match (boundary case)
- missing fields (defensive zero-state)

## Drive-by fixture sweep

`crates/caco-tui/src/app.rs` — 4× `SessionKickedModal { ... }`
literals at lines 53594, 53638, 53671, 53709 missing the
`tmux_history_limit`/`tmux_history_size: None` fields per the
bd-7ef076/bd-ab1c38 schema. Same wave class I fixed in summary
0018 — got rebased away again at some point. Sweep done with the
same Python regex pattern (insert before closing `        });`
after `tmux_socket:` line).

The bd-29bf2b merge-queue gate (cargo check --workspace --tests
abort_on_failure: true) should prevent this class from recurring
once that mixin is loaded by all 11 persistent agents on next
config reload — but it landed AFTER the recent reintegrates that
introduced these regressions.

## Smoke test

`CACO_CONFIG=~/.cacophony/config.yaml caco bd operator-actions`
on cluster:
- No flags: returns 1 row (bd-828c12 [operator-action] pocket4
  sops-nix). Confirms label+prefix filter is correctly EXCLUDING
  bd-2c399b (no label, no prefix), bd-6b7b30 (about operator-
  action workflow but not itself flagged), bd-2c7488 (unrelated
  feature).
- `--max-age 1h` on the same dataset returns "no beads found"
  (bd-828c12 is 9h old — correctly filtered out).
- `--json` returns standard `{data:{beads:[...]}}` envelope.

## Verification

- `cargo test-small`: 4249/4249 (after the SessionKickedModal
  drive-by sweep).
- `cargo clippy --workspace --no-deps`: zero warnings.
- `cargo test -p caco-cli --lib is_operator_action`: 1/1.
- Manual smoke via locally-built `target/debug/caco`: all 3 modes
  (default, --max-age, --json) behave as specified.

## Operational impact

- Operators get a one-line command for the `[operator-action]`
  subset: `caco bd operator-actions`. Replaces the implicit
  `caco bd list | grep -F '[operator-action]'` convention.
- MCP-enabled (`mcp_enabled: true, agent_safe: true`) so other
  agents can also use it as a triage primitive.
- Forward-compat: as `operator-action` migrates from title-prefix
  convention to a first-class label (per bead description "the
  value is in formalizing the 'these need a human' subset"),
  `is_operator_action_bead` already prefers labels and the
  title-prefix path is the legacy fallback.

## Deferred (not in scope)

- `--notify` operator inbox/SMS digest (bead description
  mentions; multi-system integration, separate slice).
- Server-side endpoint dedicated to operator-action queries
  (current client-side filter is fine for the queue sizes we
  see; the 200-bead pull is sub-100ms).
- Sort/group by node when the bead carries node hints in
  description (nice-to-have; deferred as a polish slice).

## Next

Reintegrate, close bd-020bc1, idle.
