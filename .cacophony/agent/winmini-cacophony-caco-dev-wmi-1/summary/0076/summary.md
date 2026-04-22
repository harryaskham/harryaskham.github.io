# Session summary — bd-2e2be3: project status beads_total includes drafts; new beads_draft field

## Goal

Issue 1 of bd-2e2be3 (test-user-hel): `caco project
status` `beads_total` silently excluded draft beads.
On cacophony itself: 53 reported vs 81 actual = 35%
undercount. Drafts can be promoted to open at any time
so they're real load.

(Issue 2 — JSON envelope shape audit across surfaces —
deferred to a follow-up bead. Scope is broader than this
single fix and requires a cross-cutting JSON contract
decision.)

## Bead(s)

- `bd-2e2be3` — test-user filed. Closing for issue 1;
  description-append captures issue 2 as deferred.

## Before state

```
$ caco project status --project cacophony --json
{ "beads_total": 53, "beads_open": 2,
  "beads_in_progress": 1, ... }

$ caco bd list --json | jq '.data.beads | length'
81  # 28 draft + 50 closed + 2 open + 1 in_progress
```

## After state

```
$ caco project status --project cacophony --json
{ "beads_total": 81, "beads_open": 2,
  "beads_in_progress": 1, "beads_draft": 28, ... }

$ caco project status --project cacophony       (text)
beads: 81 total, 2 open, 1 in progress, 28 draft
                                          ^^^ only when > 0
```

## Diff summary

- 3 files touched, +60 / −15:
  - `crates/caco-daemon/src/beads.rs::ProjectBeadSummary`:
    - new `draft: usize` field with `#[serde(default)]`
      so older peer payloads deserialize cleanly.
    - `total = open + in_progress + blocked + closed +
       draft + permanent` (was: omitted draft + permanent).
    - All 3 construction sites updated; both unit tests
      updated to include the new field.
  - `crates/caco-daemon/src/lib.rs::ProjectDetail`,
    `::ProjectStatusSummary`:
    - new `beads_draft: usize` field on both structs.
    - `(beads_total, beads_open, beads_in_progress)`
      3-tuple → 4-tuple including `beads_draft` at both
      handler sites.
    - Forward-from-authoritative path extracts
      `p["draft"]` when present, zero-fills otherwise
      (graceful for older peers).
  - `crates/caco-cli/src/lib.rs::dispatch_project_status`:
    text output now appends `, N draft` when N > 0.
    Older daemons (no `beads_draft` field) read 0 via
    `as_u64().unwrap_or(0)` — no UI regression.

## Verification

- `cargo build --bin caco`: clean.
- `cargo test-small`: 57 passed.
- `cargo test -p caco-daemon --lib beads::`:
  108 passed (incl. 2 ProjectBeadSummary fixture tests
  that exercised the new `draft` field).
- Live verification deferred to operator daemon
  restart — daemon must reload to emit the new field.
  Backward-compat verified by code review:
  - Old daemon → new CLI: `as_u64().unwrap_or(0)` returns
    0; text output matches old format.
  - New daemon → old CLI: extra field ignored.
  - Old peer → new daemon (proxy path): `p["draft"]`
    missing → 0; serde `#[serde(default)]` on
    ProjectBeadSummary handles deserialization.

## Operator-takeaway

Pattern: when adding a new field to a daemon-emitted
struct that's also forwarded peer-to-peer, the
`#[serde(default)]` annotation is essential — otherwise
older peers fail deserialization the moment a newer
daemon proxies through them. The `as_u64().unwrap_or(0)`
on the CLI side gives the same forward-compat for text
consumers.

The 4-tuple shape `(total, open, in_progress, draft)`
is getting unwieldy; a 5th counter would push toward a
dedicated struct. Consider extracting `BeadCounts {
total, open, in_progress, draft }` next time the
counter set grows.

Issue 2 (JSON envelope shape audit) is real and worth a
P3 bead but needs a cross-cutting decision: standardise
on flat `{ <resource>: [...] }`, `{ data: { <resource>:
[...] } }`, or `{ ok: true, data: [...] }`. Currently
all 3 shapes coexist. Recommendation: pick the latter
(`ok` + `data` + array) as it composes with the existing
SuccessEnvelope/ErrorEnvelope split. Filing follow-up
bead with operator discretion to pick the convention.
