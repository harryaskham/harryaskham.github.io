# Session summary — bd-a66641: caco mcp rejects unknown positional subcommand

## Goal

Eliminate the WITHIN-NAMESPACE drift where top-level `caco
mcp` silently swallowed any positional arg and returned the
full 170-tool catalog while sister surfaces (`caco bd mcp
bogus`, `caco fleet mcp bogus`, etc.) were already
gold-standard with `unknown subcommand 'bogus' for 'caco bd
mcp'`.

## Bead(s)

- `bd-a66641` — `caco mcp top-level vs sub-namespace
  WITHIN-NAMESPACE DRIFT — caco mcp silently swallows
  positional args ... while caco bd mcp bogus errors
  gold-standard ... ; caco mcp parent emits CATALOG content
  while caco bd mcp parent emits HELP-as-JSON ... ; 170-tool
  catalog envelope {version,tools} 4th no-ok surface`.

## Before state

- `caco mcp bogus` → exit 0, full 170-tool catalog (silent).
- `caco mcp bogus extra1 extra2` → same.
- `caco mcp install` (operator thinks they're installing) →
  catalog instead of error.

## After state

The `[command] if command == "mcp"` dispatch arm now checks
`parsed.positionals.first()` before calling
`generate_mcp_metadata`:

- `caco mcp bogus` →
  `error: unknown subcommand 'bogus' for 'caco mcp'.
  Allowed: stdio` (exit 1).
- `caco mcp` (no positional) → catalog as before, behaviour
  unchanged.
- `caco mcp stdio` → unchanged (separate dispatch arm).

The wording mirrors the sister-surface `caco bd mcp bogus`
gold-standard exactly, plus the `Allowed: ...` inline
allowed-values clause from bd-d492dd / bd-548e77 cohort.

## Diff summary

- `crates/caco-cli/src/lib.rs`:
  - `[command] if command == "mcp"` dispatch arm: replaced
    bare `generate_mcp_metadata()?` with a two-line block
    that returns the unknown-subcommand error when a
    positional is present, otherwise calls
    `generate_mcp_metadata()` as before.
  - 1 new test:
    `caco_mcp_rejects_unknown_subcommand` — runs
    `caco mcp bogus` and asserts both substrings
    (`"unknown subcommand 'bogus' for 'caco mcp'"` and
    `"Allowed: stdio"`).
- `cargo test -p caco-cli --lib
   caco_mcp_rejects_unknown_subcommand`: pass.
- `cargo test-small`: 175 pass.

## Operator-takeaway

Issues 2 and 3 of the bead were considered and **deliberately
deferred**:

- **Issue 2** (parent emits catalog rather than help-as-JSON)
  — the catalog IS the documented purpose of top-level
  `caco mcp` per the spec docstring ("Inspect generated MCP
  metadata"). Changing it to help-as-JSON would break the
  primary purpose of the surface. The bd-02c5f7 family
  applies to subcommand-tree branches, not to surfaces
  whose entire reason for existing is to emit a catalog.
- **Issue 3** (catalog envelope `{version, tools}` lacks `ok`)
  — this envelope is a wire format consumed by external MCP
  clients. Adding an `ok` field is a breaking change for
  every downstream MCP integration. If a uniform
  cross-surface `ok` policy is desired, it needs a
  coordinated rollout / version-negotiation, not a
  point-fix here. Worth its own bead with operator/MCP-client
  consultation.

This shipped fix addresses only the actionable bug (Issue 1)
without disturbing the catalog wire format. The
gold-standard validator wording is now consistent across
top-level `caco mcp` and every sub-namespace `caco X mcp`
surface — operator muscle-memory transfers cleanly.
