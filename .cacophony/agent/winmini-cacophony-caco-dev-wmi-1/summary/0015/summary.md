# Session summary — Daemon DB schema migration audit (bd-2a2f96)

## Goal

Cross-node version drift (helsinki v1.2.489 vs ms-dev /
winmini v1.2.460 — 29 versions apart) raised the question:
how does the older binary safely read/write the newer
schema, or vice versa? bd-2a2f96 asked for an audit and a
written compat policy.

## Bead(s)

- `bd-2a2f96` — Daemon DB schema migration story unclear
  (P2 bug → investigation)

## Before state

- No central documentation of the migration model.
- 16 `ALTER TABLE … ADD COLUMN` sites scattered across
  `crates/caco-daemon/src/{messaging,dynamic_registry}.rs`
  and `crates/caco-beads/src/store.rs`.
- Compat properties had to be reverse-engineered per-call-
  site by reviewers.

## After state

- New `docs/investigations/bd-2a2f96-daemon-db-schema-
  migration-story.md` (10.5 KB).
- TL;DR: **additive-only, idempotent, forward-and-backward
  compatible at the schema layer.** Risks are all semantic
  (older binary writes NULL/DEFAULT for newer policy
  fields), not corrupting.
- Audit table of all 16 ALTERs with bead provenance.
- Failure-mode analysis covering newer-on-older,
  older-on-newer, read paths, write paths, semantic drift.
- Proposed compat policy: schema-additive only; no
  DROP/RENAME/type-change without explicit version gate.
- Recommendations split immediate / medium / long.

## Diff summary

- Files touched (+10498 / -0):
  - `docs/investigations/bd-2a2f96-daemon-db-schema-
    migration-story.md`: investigation artefact.

(No code changes — pure investigation.)

## Operator-takeaway

For the immediate v1.2.460 ↔ v1.2.489 question across the
mesh: **the version-drift is safe at the schema layer.**

- Newer binary on older DB: self-heals via `init_tables`'s
  `CREATE TABLE IF NOT EXISTS` + pragma-gated `ALTER`s.
- Older binary on newer DB: SELECT/INSERT use explicit
  column lists, all newer columns are NULLable or have
  DEFAULTs, so older binary works against newer schema.
- The remaining risk is **semantic**, not corrupting:
  older binaries can't enforce policy invariants
  introduced in later versions (e.g. won't populate
  `dispatch_target_node`, `delivered_at`, etc).

The orthogonal "older binary missing runtime features"
concern is tracked under bd-5c2a98 (the symptom bead this
investigation pairs with).

## Three concrete follow-ups identified

1. `dynamic_registry.rs:163` swallows duplicate-column ADD
   errors via `let _ =`; should use the pragma-probe gate
   pattern for explicit intent. Behaviour is identical
   today.
2. Each migration site should log
   `bd-XXXX: migrating <table>.<column>` for post-startup
   visibility.
3. `caco doctor schema` probe (would compose with
   bd-262bd5 caco doctor self-diagnostics) — compare
   `pragma_table_info` against binary's expected column
   list and report drift.

These were **not** implemented in this slice; the bead
asked for the audit + written policy, which is now
shipped. Follow-ups can be filed as separate beads if
operator validates direction.
