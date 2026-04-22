# Session summary — caco daemon schema-info subcommand (bd-f3968d)

## Goal

bd-2a2f96 audit recommended a `caco doctor schema` probe to
detect drift between binary-expected schema and live DB
schema. This bead is a scope-down: ship just the read-side
building block — print the current schema state.

## Bead(s)

- `bd-f3968d` — [bd-2a2f96 follow-up] caco daemon schema-info
  command (P3 feature)

## Before state

- No CLI surface to inspect the daemon / beads SQLite schema.
- Operators had to use `sqlite3 daemon.db .schema` directly.

## After state

- New CLI subcommand: `caco daemon schema-info [--db
  daemon|beads|all] [--json]`.
- Read-only: opens DBs, runs `pragma_table_info` per table,
  prints a compact ASCII summary or `--json` for tooling.
- Default `--db` value: `all`.
- Missing-DB case is annotated cleanly (not an error).

## Diff summary

- Files touched (+165 / −1):
  - `crates/caco-cli/Cargo.toml`: add `rusqlite.workspace =
    true`.
  - `crates/caco-cli/src/lib.rs`:
    - new CommandSpec leaf for `daemon schema-info`,
    - new dispatch arm,
    - new `dispatch_daemon_schema_info` (~110 LOC),
    - 3 unit tests (rejects bogus filter, missing-DB
      reports cleanly, initialised DB enumerates columns
      with type/NOT NULL/DEFAULT).

### Drive-by

- `crates/caco-daemon/src/beads.rs:3238`: clippy
  `useless_format` lint re-introduced upstream of my change
  (same fix needed last cycle for bd-bd2545); replaced
  `format!("...")` with `"...".to_string()`.

## Verification

- `cargo build -p caco-cli`: clean.
- `cargo test -p caco-cli --lib schema_info`: 3 pass.
- `cargo test-small`: 56 pass.
- `cargo clippy -p caco-cli --lib --tests -- -D warnings`:
  clean.

## Operator-takeaway

Building block. Drives sample output:

```
db: daemon (/home/op/.cacophony/daemon/daemon.db)
  table: project_messages
    columns:
      - id TEXT NOT NULL
      - ts TEXT NOT NULL
      - scope TEXT NOT NULL DEFAULT 'project'
      - expires_at TEXT
      - reply_to TEXT
      - visibility TEXT
      - delivered_at TEXT
      ...
```

Future `caco doctor schema` (yet-unfiled, third follow-up
from bd-2a2f96) can compose this output against a baked-in
expected-schema constant to flag drift between binary
version and live DB version.

This closes follow-up #3 of 3 from bd-2a2f96 (the building
block for it; the drift-detection layer remains a separate
slice).
