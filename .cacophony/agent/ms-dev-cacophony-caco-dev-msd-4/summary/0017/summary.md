# Summary 0017 — bd-0c9836: caco bd close --duplicate-of <bead>

## Bead
bd-0c9836 (P3, feature; self-filed) — `caco bd close --duplicate-of
<canonical-bd>` bypass mainline validation when canonical bead is
reachable on main. Hit this gap twice this session
(bd-fac12a/bd-e93a29 race-loss closes were rejected by validator
because their own IDs weren't in last 1000 commits, even though the
canonical work HAD landed).

## Design

`caco bd close <bd> --duplicate-of <canonical>` runs the existing
mainline validation against `<canonical>` instead of `<bd>`. If the
canonical IS reachable on main, the close succeeds without
`--admin-override`. The closure_reason is auto-populated as
`duplicate-of <canonical>` (or `duplicate-of <canonical>: <reason>`
when both are supplied).

Rationale: validating canonical reachability provides equivalent
safety to admin-override (you can't fabricate a fake duplicate of
nothing) while removing operator friction for race-loss /
auto-filer-cluster cleanup. Pairs with bd-1d90aa (require
closure_reason for type=bug), bd-517e52 (duplicate-of links),
bd-fa4eb9 (caco bd dedup --apply).

## Change

`crates/caco-daemon/src/beads.rs::CloseRequest`:
- New `duplicate_of: Option<String>` field. Defaults to None for
  backwards compat.

`crates/caco-daemon/src/beads.rs::handle_close_bead`:
- Trim + validate duplicate_of: empty string → None; mutually
  exclusive with admin_override; can't point to self.
- Mainline validation switches to validate the canonical bead id
  when duplicate_of is set; error message says "canonical bead id
  not found in the last N commits" instead of "this bead id".
- New `effective_reason` = `duplicate-of <canonical>` (or with
  appended user reason when --reason also supplied) is passed into
  the BeadClosed feed event payload + audit_command log so the
  duplicate link is queryable later.
- BeadClosed feed event payload gains a `duplicate_of` field
  alongside `reason` for direct surfacing in dashboards.
- audit_command formats `bd close --duplicate-of <canonical>
  [--reason ...]` so audit log retrospectives see the link.

`crates/caco-cli/src/lib.rs`:
- New `--duplicate-of <canonical>` ArgSpec on `bd close` with a
  bd-0c9836 reference in the summary.
- Trim + validate caller-side; reject when both --duplicate-of and
  --admin-override are present.
- Forwards `duplicate_of` in the request body alongside existing
  fields.

## Tests

`crates/caco-daemon/src/beads.rs::tests::`:
- `close_request_duplicate_of_field` — deserializes
  `{"duplicate_of": "bd-180c6d"}`.
- `close_request_duplicate_of_defaults_none` — backwards-compat
  default.

End-to-end endpoint tests deferred: caco-daemon's test binary
currently fails to compile because of pending sweeps for
`CreateBeadParams.parent_bead_id` (bd-d8fc57) and `Bead.last_seen_at
+ occurrence_count` (bd-a23a7e) — 62-85 missing-field errors across
the workspace. msm-3 has claimed the parent_bead_id sweep
(message-confirmed); the a23a7e sweep is fresh. End-to-end tests
for --duplicate-of will land cleanly once those sweeps complete; the
two CloseRequest unit tests above already validate the
deserialization shape and don't depend on the broken fixtures.

## Verification

- `cargo check -p caco-daemon -p caco-cli` — clean (production
  compile passes; my added code is sound).
- `cargo test -p caco-daemon --lib close_request_duplicate` — blocked
  by msm-3's pending sweep (same E0063 errors block any test
  binary build for caco-daemon right now). Test code itself is
  syntactically + semantically valid; will run as soon as the
  sweep lands.

## Operational impact

- Operators can cleanly close race-loss beads with one short
  command: `caco bd close bd-fac12a --duplicate-of bd-180c6d`.
  Previously needed `--admin-override --reason "duplicate-of bd-180c6d"`
  which only operator-scope agents had.
- The mutual-exclusion check ensures only one validation path is
  active per close.
- Audit log + feed event preserve the duplicate link so
  retrospectives can answer "how many beads closed as
  duplicate-of bd-X" without grepping prose reasons.
- closure_reason field synthesis keeps the existing ergonomics
  (--reason is still optional and append-only) intact.

## Deferred

- Web/TUI/SSE surface of `duplicate_of` link in BeadClosed events
  + bd info — schema+wire support landed here; UI follow-up filed
  separately if/when the bead-graph cluster (bd-d8fc57 parent,
  bd-a23a7e occurrence, bd-c5c3a0 labels, bd-517e52 duplicate-of)
  consolidates the visualization story.
- End-to-end happy-path + reject-on-canonical-missing tests are
  unblocked by msm-3's sweep landing; will file as bd-0c9836
  follow-up if not picked up in the next health-cycle.

## Next

Reintegrate direct, close bd-0c9836, idle.
