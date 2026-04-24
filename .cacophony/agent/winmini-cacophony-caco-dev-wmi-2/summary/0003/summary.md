# Session summary — bd-bc3d7d: caco ls validator parity for --kind/--project/--agent

## Goal

Apply the bd-2dc0c3 validator pattern to `caco ls`: reject
bogus values for `--kind`, `--project`, and `--agent` so all
three filters give actionable errors instead of silently
degrading to header-only output.

## Bead(s)

- `bd-bc3d7d` — `caco ls SILENT on bogus filters: --kind bogus,
  --project nonexistent, --agent bogus all return header-only
  with no error`.

## Before state

- `caco ls --kind bogus` → header-only, no error.
- `caco ls --project nonexistent` → header-only, no error.
- `caco ls --agent bogus` → header-only, no error.

Same family as the bd-2dc0c3 issue on `caco ps`. The bead's
secondary observations — `--json` envelope shape divergence
(`{ok,entries,count,runtime_root,node}` vs the standard
`{ok,data,meta}`) and the unscoped 444-line default — are
out of scope here; this fix is the silent-filter
acceptance item.

## After state

`dispatch_ls` declares `KNOWN_KINDS = ["checkout", "beads",
"agents", "logs", "state", "pki", "tokens", "diagnostics"]`
matching the help-text values in `LS_ARGS`, plus three
upfront validators that mirror bd-2dc0c3's pattern:

- `caco ls: unknown --kind 'bogus'. Valid kinds: checkout,
  beads, agents, logs, state, pki, tokens, diagnostics`.
- `caco ls: project 'X' is not configured. Configured
  projects: cacophony, …`.
- `caco ls: agent 'X' has no on-disk checkout under
  <root>/agents. Use 'caco agent list' to see known agents.`.

The `--agent` validator does a cheap filesystem scan of
`<runtime_root>/agents/<project>/<id>` rather than a daemon
round-trip. The dispatcher already walks the agent runtime
tree to assemble results; rejecting an unknown agent ID
upfront just short-circuits a guaranteed-empty walk and
gives the operator a clear next step.

## Diff summary

- `crates/caco-cli/src/lib.rs`:
  - `dispatch_ls`: added `KNOWN_KINDS` const + three upfront
    validators (`--kind`, `--project`, `--agent`).
  - 1 new test:
    `dispatch_ls_validates_kind_project_and_agent_filters` —
    source-greps the dispatcher body for the validator error
    wording and the `KNOWN_KINDS:` declaration.
- `cargo test -p caco-cli --lib
   dispatch_ls_validates_kind_project_and_agent_filters`: pass.
- `cargo test-small`: 162 pass.

## Operator-takeaway

`caco ls` and `caco ps` now share the same upfront-validator
pattern. The same shape extends naturally to any list-by-X
subcommand whose filters reference an enum or a known set —
the cheap path is two const arrays + three branches at
function entry.

Out of scope (kept explicit so a future claimant doesn't
think it landed):

1. `--json` envelope unification: `caco ls` still emits
   `{ok,entries,count,runtime_root,node}` instead of the
   `{ok,data,meta}` family. That's a daemon-side response
   shape question worth its own bead.
2. Default-invocation scope: `caco ls` with no filters still
   emits 444 lines. A `--limit` default or a top-level
   summary view would be a separate UX feature.

Both are noted in the bead description but deliberately not
addressed here so the silent-filter fix lands cleanly.
