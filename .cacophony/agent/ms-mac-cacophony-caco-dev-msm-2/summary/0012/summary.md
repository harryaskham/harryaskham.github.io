# Session summary — Bead surface dependency-encoding spec (bd-2a3aeb)

## Goal

Pin the on-disk file format for "bead surfaces" — collections
of beads exchangeable between projects, exportable as
snapshots, importable as starting sets — with explicit
dependency-edge encoding and deferred-resolution semantics.

## Bead(s)

- `bd-2a3aeb` — Design bead surface for encoding dependencies
  (P2, bead-system, dependencies, file-format)

## Before state

- Only on-disk surface was the per-project `.beads/issues.jsonl`
  consumed by the daemon's bead store.
- Dependency edges encoded as `{issue_id, depends_on_id, type,
  created_at}` inline in each bead — but no spec, no
  cross-project transfer, no deferred-resolution semantics.
- bd-cf99b7 / bd-fc60ff exposed that issues.jsonl writes can
  SIGBUS git readers because there's no atomic-rename
  protocol.
- No story for forward-references when operators quick-file a
  wave of related beads.

## After state

New `docs/epics/bd-2a3aeb-bead-surface-dependency-spec.md`
(~17.8KB, 12 sections) covers:

- **Goals/non-goals** (§2): single canonical JSONL format,
  three edge types, deferred resolution at load time,
  cross-surface transfer, conflict-safe writes, BACKWARD-
  COMPATIBLE with existing `.beads/issues.jsonl`. Explicitly
  out: graph algebra (bead store's job), ACLs, binary
  attachments, real-time push, in-memory representation.
- **Container layout** (§3.1): `<surface>/surface.toml +
  issues.jsonl + attachments/ + README.md`. Legacy single-
  file form supported as implicit surface with defaults.
- **Manifest schema** (§3.2): `surface_id`, `spec_version`,
  `project_scope`, `name`, `description`,
  `deferred_resolution`, `bead_id_prefix`.
- **Bead encoding** (§3.3): JSONL one-per-line, reuses
  existing `caco_beads::model::Bead` shape.
- **Dependency edge schema** (§4): existing fields + two
  optional extensions (`pending_until`, `note`); three edge
  types (`blocks`, `relates`, `parent`) with explicit
  semantics + resolver behaviour table.
- **Deferred resolution** (§5): three modes (strict / warn /
  silent), resolver pseudocode, idempotency + monotonicity
  guarantees, cycle-detection-window discussion +
  `cycle_warning` flag in in-memory model.
- **Conflict-safe write protocol** (§6): 5-step
  `lock + tmp + fsync + rename + unlock` that closes the
  bd-fc60ff SIGBUS race; explicit Windows + cross-mount
  fallback.
- **Examples** (§7): minimal surface, cross-project import,
  forward-reference quick-file, epic with parent + blocks.
- **Migration** (§8): no file rewrite required; `caco bd
  surface init` produces `surface.toml` next to existing
  `issues.jsonl`; round-trip preserves legacy lines exactly.
- **Validation** (§9): unique IDs, edge symmetry,
  `deferred_resolution` enum check, `spec_version` known,
  no `blocks` cycles, `pending_until` parses, project_scope
  matches path inference (warn-only).
- **Trade-off matrix** (§10): JSONL vs SQLite, deferred-
  resolution cycle window, three edge types only,
  `pending_until` overhead, atomic-rename FS limit, surface
  import dedup key choice.
- **Out of scope** (§11): diff/merge UI, cycle-break
  suggestions, encrypted surfaces, schema versions > 1.0.
- **Acceptance map** (§12) — ticks each of the bd-2a3aeb
  acceptance criteria explicitly:
  - (1) data structure/format → §3
  - (2) dependency encoding → §4
  - (3) deferred resolution → §5
  - (4) examples with various scenarios → §4.4 + §7

Plus pins implementation hooks for follow-up beads:
`caco bd surface {init,export,import,validate}` CLI shape;
`surface.toml` parser; `pending_until` serde field;
resolver pass integration; atomic-rename protocol (likely
shares code with bd-fc60ff's mitigation).

## Diff summary

- Files touched:
  - `docs/epics/bd-2a3aeb-bead-surface-dependency-spec.md`
    (new, ~17.8KB)
- Tests: +0 / -0 (design doc, no code)

## Operator-takeaway

Read before claiming any of the implementation work this
unblocks. Key design decisions to flip-or-keep:

1. **Three edge types only** (`blocks`, `relates`, `parent`).
   Resists scope creep. New types earn their place via
   repeated demand; in the meantime nuance encodes as
   `relates` + `note`.
2. **JSONL not SQLite.** Full-file rewrites on edit are
   already what the reconciler does; atomic-rename keeps
   reads safe; tooling stays grep-friendly. Easier to
   recover from disasters by hand.
3. **Warn is the default `deferred_resolution` mode.**
   Forward references load with `pending_until: "first_seen"`
   and a feed warning. Strict mode for production surfaces
   that need graph integrity guarantees; silent for ephemeral
   draft branches.
4. **Conflict-safe write protocol is explicit and POSIX-
   atomic.** Closes the bd-fc60ff race directly: surfaces
   never see truncated input mid-write because the rename
   swaps the inode atomically. Implementation likely shares
   code with the bd-fc60ff mitigation that helsinki-side
   agents are working on.
5. **Backward-compatible.** Existing `.beads/issues.jsonl`
   files load as implicit surfaces with no migration. New
   fields (`pending_until`, `note`) round-trip absent in
   legacy bead lines.

This is the **fourth** design doc this session (after
bd-1975be Codespaces architecture, bd-db60a1 timeline view,
bd-f32dda key distribution). The 11-12-section pattern is
solid; future design beads should adopt.

15th bead closed this session (cumulative). 8 in this turn.

Honored constraints:
- No `cargo test` (design doc).
- No daemon / sidecar code touched.
- Operator no-narrator rule honored — claim + close speaks
  issued by msm-2 directly.
- Operator no-docker note unaffected (bead-format work).
