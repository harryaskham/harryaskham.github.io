# Session summary 0027 — bd-62a9d0: caco bd dedup (slice 1, read-only)

## Goal

Give operators a one-shot CLI to scan an existing draft pool for
duplicate beads, addressing the 988-draft accumulation observed
tonight without requiring full triage workflow.

## Bead(s)

- `bd-62a9d0` slice 1 — read-only Jaccard scanner.

## Before state

- 988 drafts accumulated; the only way to find duplicates was
  manual grep + `caco bd show` per candidate.

## After state

- New `caco bd dedup` subcommand. Default behavior:
  `caco bd dedup --by title-similarity --status draft --limit 500`.
- Scans beads matching the status filter, groups by Jaccard
  similarity (≥0.85) on lowercased title word-sets (≥3 chars).
- Prints groups with canonical (oldest) marked, members listed.
- `--json` emits `{status, by, threshold, scanned,
  groups:[{size, canonical, members:[{id, title}]}]}`.
- Read-only — slice 1 is dry-run only. Mutation (close-as-
  duplicate-of, `--apply`) deferred.
- Other heuristics (`stderr-signature`, `description-hash`)
  rejected with a clear "deferred" message.

## Diff summary

- Commit: `66b72a5b`.
- Files (1): `crates/caco-cli/src/lib.rs` (+201 lines).
- `cargo build -p caco-cli` + clippy: clean.

## Operator-takeaway

Run `caco bd dedup` to see candidate duplicate groups in the
draft pool. Use `--limit N` to widen the scan. JSON mode is
script-friendly — pipe groups into `caco bd close` to manually
collapse. The `--apply` mode (auto-collapse) is filed as a
follow-up bead so the canonical-choice heuristic can be reviewed
on real groups first.
