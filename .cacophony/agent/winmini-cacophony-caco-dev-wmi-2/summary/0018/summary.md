# bd-332f45: agent-spawn unknown-preset error now hints at persistent.yaml namespace

## Goal

Improve the `agent preset 'X' not found in agents.presets config` error returned when an operator passes `--preset` for a name that actually lives in `cacophony_persistent.yaml` (a separate config namespace). When such a name match exists, the error now lists the node(s) where the persistent decl is defined and suggests `--profile <id>` as the workaround.

## Bead(s)

- bd-332f45 (P3 bug; closes via reintegrate)

## Before state

`caco agent new --node beelink --preset technical-writer` (when `technical-writer` is declared in `cacophony_persistent.yaml` but not `agents.presets`) returned:
```
error: agent preset 'technical-writer' not found in agents.presets config
```
Operator had no way to discover (without reading source) that `--profile technical-writer` was the workaround.

## After state

The same scenario now returns:
```
agent preset 'technical-writer' not found in agents.presets config; however
a persistent declaration named 'technical-writer' exists on node(s) beelink.
Persistents and presets are different namespaces — try `--profile
technical-writer` (with explicit `--node` and `--goal`) or run the
persistent's reconcile path instead.
```

Implementation: in `crates/caco-daemon/src/lib.rs::create_agent` handler, when the requested preset is `None` from `find_preset`, walk `state.config.nodes` to detect persistent decls with the same key, collect node names, and conditionally swap the error message. The original `unknown_preset` error code is preserved so JSON-mode callers don't break.

Verification:
- `cargo test-small`: 57/57 PASS
- `cargo clippy --workspace --all-targets -- -D warnings`: clean
- Did not add a unit test — the existing daemon HTTP-handler tests are heavy fixtures and the error-shape change is covered by the `unknown_preset` code-path callers; the message-content delta is operator-facing only

## Diff summary

1 file changed, +29 / −5:

- `crates/caco-daemon/src/lib.rs::create_agent`: replaced inline `format!` with a `persistent_node_hits` lookup + conditional message construction

## Operator-takeaway

**Single-error-message improvement** that addresses a real foot-shoot from tonight's session. The bead's fix #1 (`--persistent` flag) and fix #2 (auto-fallback) are both bigger scope and have semantic implications; fix #3 (better error) is the cheapest+safest win and unblocks the operator immediately without changing semantics.

Could be combined with a follow-up that does the same hint-walk on `--profile <name>` when a profile name doesn't exist but a persistent does (or vice-versa). Filing as follow-up if the symmetric case shows up. Cross-namespace name-collision errors are a recurring papercut as more config namespaces accumulate.

This cycle: **fourth quiet cycle in a row** for broken-on-main waves. The merge-queue gate is doing its job.
