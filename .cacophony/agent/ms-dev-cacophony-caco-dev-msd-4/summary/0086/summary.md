# Session summary — exact @ target matching

## Goal

Fix `@` shorthand resolution so an exact entity match is not treated as ambiguous just because other agents have IDs with the same text as a prefix.

## Bead(s)

- `bd-33694b` — where @ syntax is exact text match for one entity, do not treat as ambiguous

## Before state

- Failing tests: no regression covered exact node names versus agent ID prefixes.
- Relevant metrics: `caco tui @helsinki` could fail as ambiguous because the node `helsinki` matched exactly while many live agent IDs began with `helsinki-...`.
- Context: fuzzy `@` resolution is shared CLI behavior for node/project/agent shorthand and still needs ambiguity protection for true exact collisions.

## After state

- Failing tests: none in validation.
- Relevant metrics: added exact-match precedence in `resolve_at_target` and its test mirror: a single exact node/project/agent match wins over prefix-only fuzzy matches; exact collisions such as node short-name plus project still return the existing ambiguity guidance.
- Context: docs now describe the exact-match behavior and preserve explicit `@node:`, `@project:`, and `@agent:` disambiguation for real conflicts.

## Diff summary

- Commits: `e28b77d5c`
- Files touched: `crates/caco-cli/src/lib.rs`, `SPEC.md`, `README.md`, `AGENTS.md`, `docs/cli.html`
- Tests: `cargo fmt --all -- --check`; `cargo test -p caco-cli fuzzy_resolve_exact --lib`; `cargo test -p caco-cli fuzzy_resolve_ambiguous_across_namespaces --lib`; `cargo check -p caco-cli`; `cargo clippy -p caco-cli`; `docs/validate-pages.sh`; `cargo test-small`.
- Behavioural delta: `@helsinki` now resolves to node `helsinki` despite `helsinki-*` agent prefixes, while true exact namespace collisions remain explicit-prefix errors.

## Operator-takeaway

The `@` shorthand now behaves the way operators expect: exact names win over fuzzy prefixes, without weakening ambiguity safety for real collisions.
