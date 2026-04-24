# Session summary — `caco config show --path` JMESPath filter

## Goal

Close bd-deec53: add a `--path` flag to `caco config show` that lets
power users drill into config with JMESPath expressions, eliminating
the need to pipe through yq/jq for deep inspection.

## Bead(s)

- `bd-deec53` — caco config show: --path JMESPath / jq expression for
  power users (sibling of bd-2b096e issue 4)

## Before state

- `caco config show` emits the full config (or --section/--node-filtered
  subset) as YAML or JSON. Deep field inspection requires piping through
  external tools (`yq`, `jq`).

## After state

- New `--path` flag accepts a JMESPath expression applied after
  --section/--node filtering. Forces JSON output.
- Empty value and invalid JMESPath expressions rejected with bd-id
  breadcrumbs.
- Examples: `--path 'nodes[*].name'`, `--path 'version'`,
  `--path 'length(nodes)'`.
- New dep: jmespath = "0.5.0" for caco-cli.
- 5 unit tests, all wrapped in 8 MiB-stack helper (bd-c5fa50 pattern).

## Diff summary

- Commit: bd-deec53 caco config show --path
- Files: `crates/caco-cli/src/lib.rs` (+171), `crates/caco-cli/Cargo.toml` (+1),
  `Cargo.lock` (+28)
- Tests: 5 new, all passing. cargo test-small green (182 pass, 1 pre-existing
  failure in caco-web unrelated — filed as bd-ee6b07).

## Operator-takeaway

`caco config show --path '<jmespath>'` is now the one-shot way to drill
into a specific config field without piping through yq/jq. Composes
with `--section` and `--node` filters. Forces JSON output (so output
is pipe-stable). Examples:

  caco config show --path 'nodes[*].name'
  caco config show --path 'nodes[?name==`"helsinki"`].services'
  caco config show --path 'length(nodes)'

String literals in JMESPath filter expressions must be JSON-quoted
inside backticks (`"helsinki"` not `helsinki`) — surfaced inline in
--help so operators don't have to read the JMESPath spec.
