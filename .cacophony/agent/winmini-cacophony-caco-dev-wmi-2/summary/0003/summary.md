# Session summary — discovered-via provenance via structured labels (bd-ffb193)

## Goal

Add a structured `discovered_via` provenance field for beads so that
the prose-only "filed via caco-ctrl from <observation>" provenance
pattern in many recently-filed beads becomes queryable, surface-
renderable, and prioritisation-friendly. The bead asked for
agent / session_bead / session_event / node fields with use cases like
"prioritise beads discovered during real-user exercise" and
"spot daemon-outage symptom clusters".

## Bead(s)

- `bd-ffb193` — discovered-via provenance field (P3 feature)

## Approach decision

Two candidate designs:

1. **Schema column on `issues` table** (the bead's literal suggestion).
   Survey: 600+ `Bead { ... }` struct literal sites across the
   workspace, plus ~25 SQL bind/select sites in `caco-beads/store.rs`.
   Touches every test fixture in caco-beads/caco-daemon/caco-tui.

2. **Structured labels** mirroring the existing bd-427d4a
   `STRUCTURED_LABEL_KEYS` mechanism (the same way scheduling
   constraints `node:`, `type:`, etc. are encoded today).

Picked option 2:
- Zero schema migration, zero risk to the 600 struct-literal sites.
- Existing `--labels` and `--label` filter flags work immediately.
- TUI's `is_structured_label` rendering picks up the new keys for free.
- Round-trippable through `DiscoveredVia::to_labels` /
  `parse_discovered_via` so callers that want structured access do
  not have to pattern-match label strings themselves.

## Before state

- Provenance lived only in description prose.
  `caco bd create` had no provenance flags.
- `STRUCTURED_LABEL_KEYS` covered only scheduling
  (`type, node, provider, model, profile`).
- `cargo test-small`: 4198 tests passing.

## After state

- Four new structured label keys recognised by
  `is_structured_label`:
  - `discovered-via-agent:<agent-id>`
  - `discovered-via-bead:<bd-id>`
  - `discovered-via-session:<event-name>` (e.g.
    `reflect-session`, `manual-triage`, `test-user-exercise`)
  - `discovered-via-node:<node-name>`
- New `model::DiscoveredVia` struct + `model::parse_discovered_via`
  in `crates/caco-beads/src/model.rs`. First-value-wins on duplicate
  keys (provenance is a single point of origin, unlike scheduling
  which OR's). Round-trip via `to_labels()`.
- New CLI flags on `caco bd create`:
  `--discovered-via-{agent,bead,session,node}`. Each non-empty value
  folds into the labels array and merges with `--labels` rather than
  replacing it. Help text wired through `BD_CREATE_ARGS`.
- Filtering uses the existing
  `caco bd list --label discovered-via-session:reflect-session` —
  no new filter flag required.

Drive-by clippy fixes (peer commits broke `-D warnings`):
- `caco-daemon/store.rs` `note_delivery_tracking` docstring
  (`doc_lazy_continuation` on a `+` continuation line).
- `caco-cli/lib.rs` agent-log `--all` docstring overindent
  (`doc_overindented_list_items`).
- `caco-cli/lib.rs` manual `taken` counter →
  `.enumerate()` (`explicit_counter_loop`).

`cargo test-small`: **4207 PASS / 0 FAIL** (+9 from 4198: 6 new
provenance tests + 3 absorbed from rebased peer work).
`cargo clippy --workspace --all-targets -- -D warnings`: PASS clean.

## Diff summary

Commits this session (rebased onto `c70d37f0`):
- `9ea149c2` — model: DiscoveredVia + parse_discovered_via + 6 tests
- `fbcfdd1f` — CLI flags + drive-by clippy fixes

Files (3 / +268 / -8):
- `crates/caco-beads/src/model.rs` (+200 / -1):
  STRUCTURED_LABEL_KEYS expansion, DiscoveredVia struct,
  parse_discovered_via fn, 6 new unit tests.
- `crates/caco-cli/src/lib.rs` (+64 / -7):
  4 new ArgSpec entries, provenance-flag merge into body["labels"],
  drive-by lint fixes.
- `crates/caco-daemon/src/store.rs` (+4 / -4):
  drive-by docstring rewrite.

## Operator-takeaway

Structured-label provenance is the cheapest possible delivery path
that satisfies all use cases in the bead: queryable
(`--label discovered-via-session:test-user-exercise`), filter-friendly,
TUI-renderable, and credit-attribution-able. If a future need wants
indexed JSON columns we still have an upgrade path — the
`parse_discovered_via` boundary keeps callers shielded from the
underlying representation.

Pairs with bd-c5c3a0 (labels), bd-9006a6 (richer query), bd-2e2338
(triage workflow). The sibling beads that wanted to filter by
"who filed this and why" can now do so against any bead created
from this commit forward.
