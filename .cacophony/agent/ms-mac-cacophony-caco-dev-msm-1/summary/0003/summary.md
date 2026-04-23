# Session summary — caco prune list/run filter validation

## Goal

Close bd-cd2ec9 — sibling miss of the bd-bc52ef silent-no-op-on-unknown
family. `caco prune list/run` was returning a friendly "no prunable
agents" / "nothing to prune" line on garbage filter values
(`--state notreal`, `--node notarealnode`, `--project nonexistent`,
`--id nonexistent_id`) instead of erroring with a useful suggestion.

## Bead(s)

- `bd-cd2ec9` — [CLI honesty] caco prune list/run silently return 'no
  prunable / nothing to prune' for unknown --state, --node, --project,
  --id (sibling of bd-bc52ef family)

## Before state

All five reproduction cases from the bead silently returned the empty
result with exit 0. Operators discovered typos only by manually
diffing against `caco agent list` / `caco config show projects` /
`caco config show nodes`.

## After state

```
$ caco prune list --state notreal
error: unknown --state value 'notreal'. Allowed: pending, starting,
running, waiting, blocked, recovering, retrying, stale, stalled,
paused, completed, failed, stopped, discarded
$ caco prune list --node notarealnode
error: unknown node: notarealnode. Defined: ms-mac, ms-dev, helsinki, …
$ caco prune list --project nonexistent
error: unknown project: nonexistent. Defined: a.skh.am, cacophony, …
$ caco prune run --dry-run --id nonexistent_id
error: no agent with id 'nonexistent_id' (use `caco agent list` to enumerate known agents)
$ caco prune run --dry-run --project nonexistent
error: unknown project: nonexistent. Defined: …
```

Happy path unchanged (`caco prune list`, `caco prune list --state
completed`, `caco prune list --state stopped,completed` all work).

`cargo test-small` 57/57 PASS, `cargo clippy -p caco-cli --lib --tests`
clean. Three new unit tests on the shared validator.

## Diff summary

- Commit: 0aba4bd5
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: +3 / -0
- Behavioural delta: filter validation at the top of `dispatch_prune_list`
  / `dispatch_prune_run` via a shared `validate_prune_filters` helper.

## Operator-takeaway

Same shape as the existing bd-bc52ef family. The validator is a single
function so when the next sibling-miss surfaces (and it will — the
unrelated nudge in the bead points at the `--state` default for
`prune list` arguably hiding most reclaimable disk in `stopped` agents),
we can extend the same helper rather than re-implementing per-dispatch.

The unrelated nudge from the test-user (default `--state completed` may
be hiding the heavier `stopped`-state reclaimable rows) is NOT addressed
here — it's a default-behaviour question, not a silent-on-bad-input
bug, and the bead explicitly flagged it as a separate observation.
File a follow-up if an operator confirms that's a problem in practice.
