# Session summary 0044 — bd-ab376b: AFK-fallback choice schema

## Goal

Schema slice for AFK-fallback auto-fire on operator choices, so
cluster-ctrl-style pre-commits don't sit waiting hours when the
operator is AFK.

## Bead(s)

- `bd-ab376b` slice 1 — schema only.

## Before state

- Disk-threshold pre-commits with operator-only resolution had
  no graceful degradation. Tonight: cluster sat at threshold for
  ~50 min waiting on operator response.

## After state

- `ChoiceOption.autonomy_tier: Option<String>` field
  (`"operator-only"` | `"autonomous-low-risk"` |
  `"autonomous-reversible"` | ...).
- `ActiveChoice.afk_fallback_index: Option<usize>`.
- `ActiveChoice.afk_fallback_after_secs: Option<u64>`.
- All optional with `#[serde(default,
  skip_serializing_if = "Option::is_none")]`.
- Existing spawn-confirm call site in lib.rs updated with
  explicit `None` values.
- 16 caco-daemon::choices tests pass.

## Diff summary

- Commit: `568f32a9`.
- Files (3): caco-daemon choices.rs + lib.rs + operator_inbox.rs.
- `cargo build` and `cargo clippy`: clean.

## Operator-takeaway

Agents can now declare which option to auto-fire and after how
long, plus mark each option's risk tier. Supervisor honour of
these fields (auto-fire after timeout, audit-tag as
`autonomous-remediation`) is slice 2 — a self-contained
supervisor-side change now that the schema is settled.
