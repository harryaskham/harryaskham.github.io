# Session summary — bd-87b21c caco doctor --node wiring

## Goal
Make `caco doctor --node <name>` actually scope output (was a documented no-op).

## Bead(s)
- `bd-87b21c` — caco doctor --node flag completely inert; help promises cosmetic labelling but it doesn't fire

## Before state
- `--node` parsed by clap but never reached dispatch_doctor.
- Header always read local hostname; `--node bogus` and `--node ''` silently accepted.
- Cosmetic-labelling promise in help was a lie.

## After state
- main dispatch threads `parsed.flags.get("--node")` into dispatch_doctor.
- Empty / unknown values rejected with clean error listing configured nodes.
- Valid override propagates to: header label, config-check detail, `caco_cert::status_for_node`, `resolve_effective_daemon_listener`.
- `local_node_name` retained distinctly so future probes can opt out of the override.
- 3 new tests pin the behaviour (relabels, unknown errors, empty errors).

## Diff summary
- `crates/caco-cli/src/lib.rs` (+157 / -5): dispatch wiring, validation, 3 tests, signature update at 3 existing test call sites.
- `cargo test-small`: 151 passing.

## Operator-takeaway
`caco doctor --node helsinki` now actually scopes the output to helsinki (label, cert, daemon listener). `--node bogus` / `--node ''` produce clean errors instead of silent fall-through.
