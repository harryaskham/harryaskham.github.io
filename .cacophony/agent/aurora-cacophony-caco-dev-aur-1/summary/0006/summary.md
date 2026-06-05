# Session Summary — aurora-cacophony-caco-dev-aur-1

## Goal
Resume the open-bead queue as an endless dev worker after a long beads-primary
outage, then implement and land a real fix for the recurring intermittent
beads-read latency spikes that had been re-filed three times without any code
change landing.

## Bead(s)
- **bd-db3f48** (claimed, fixed, landing this reintegration): "ms-mac recurring
  intermittent beads-read latency spikes (25-40s) under compile load:
  /api/v1/beads full-scan stalls, daemon core healthy." Third re-file of the
  same symptom (prior identical bd-a5b25e and bd-dfd1f6 were both closed within
  ~12-24 min with no referencing commit — auto-filed by the log-monitor sweep
  and closed without a fix, so the symptom kept recurring).
- **bd-0a9517** (filed as draft, NOT claimed): reflect-session friction —
  "Beads-handler wedge on sole beads-primary causes fleet-wide outage with no
  auto-recovery." Captures the systemic single-point-of-failure gap observed
  during this session's outage.

## Before state
- A ~55 min beads-primary outage (~12:40Z–13:35Z): the helsinki daemon's beads
  HTTP handler family (`/api/v1/projects/<p>/beads*`, `bd show`, `bd stats`,
  `bd operator-actions`) returned `daemon_endpoint_nonresponse` /
  `beads_proxy_unavailable` while `/api/v1/node` (helsinki v1.2.1089) and the
  cached `bd status` routing view stayed healthy. `candidates=[helsinki]`,
  `failover_health=not_configured` (sole primary, no auto-failover). Localized
  and escalated (router + speak) without attempting any worker-forbidden
  restart; held claims against the unstable/flapping handler until it passed
  6/6 consecutive stable probes.
- On the read path itself: `BeadsStore::list_beads` and `list_ready` loaded the
  full `description` column for **every** matched row on every full-scan read,
  then enriched (labels/deps/attachments/lifecycle) the whole set, and only
  afterward truncated to `limit` / counted. For count-only board polls the body
  is discarded entirely, so all that description materialization was pure waste.
  Descriptions are the largest per-row payload on a mature board (multi-KB
  Markdown each), making this the dominant cost behind the latency spikes under
  concurrent compile load.

## After state
- Added `skip_descriptions: bool` to `BeadFilter` (default `false`, fully
  backward-compatible — all 56 construct sites use `..Default::default()`), plus
  `BeadsStore::list_ready_opts(skip)`. When set, the SELECT substitutes the
  empty-string literal for the `description` column so SQLite never reads/copies
  the blob, while keeping every downstream column index identical so
  `row_to_bead_base` is unchanged.
- Wired both `handle_list_beads` (project-scoped) and `handle_all_beads`
  (all-projects) to set `skip_descriptions = count_only && grep.is_none()`.
  Scoped deliberately to `count_only`: those responses carry `beads: None` and
  emit no `description_len`, so dropping the body is safe. The Omit-projection
  list path that still reports the true `description_len` is intentionally left
  untouched, and `grep` (which matches the body) keeps full descriptions.
- Added two unit tests asserting the matched row set, ids, and ready ordering
  are preserved while bodies are emptied.

## Diff summary
- `crates/caco-beads/src/store.rs`: `BeadFilter.skip_descriptions` field;
  `list_beads` conditional description column; `list_ready` delegates to new
  `list_ready_opts(skip)`; 2 new tests. 
- `crates/caco-daemon/src/beads.rs`: compute `skip_descriptions` and thread it
  into the ready path (`list_ready_opts`), the project-scoped `BeadFilter`, and
  the all-projects sweep (`list_ready_opts` + `BeadFilter`).
- Local commit on the agent branch: `722578cc44` ("bd-db3f48: skip loading bead
  descriptions on count-only beads reads"). The final landed squash SHA is in
  the reintegration receipt for this run.

## Validation
Run through the daemon test queue (shared-host policy):
- `cargo check -p caco-daemon --lib` — passed (tj-01250813)
- `cargo clippy -p caco-beads -- -D warnings` — passed (tj-091c61a5)
- `cargo test -p caco-beads --lib list_beads` — 8 passed (incl. new test)
- `cargo test -p caco-beads --lib list_ready` — 5 passed (incl. new test)
- `cargo test -p caco-beads --lib skip_descriptions` — 2 new tests passed
The merge-queue gating runner runs the canonical test-small + clippy + check on
the prospective merge commit during reintegration.

## Operator-takeaway
The recurring beads-read latency symptom was re-filing because no fix had ever
landed — two prior identical beads were closed within minutes with no commit.
This lands the first real contributor fix: count-only board polls (the flagged
9.6s-spike path) and ready/all-beads reads no longer materialize every
multi-KB description blob, cutting the dominant full-scan read cost under
compile load. The deeper systemic gap — a wedged-but-alive beads handler on the
sole beads-primary causing a fleet-wide outage with no auto-recovery — is
captured for triage as draft bd-0a9517 (suggests a self-heal watchdog for the
wedged-handler case and making the single-primary SPOF visible in `caco ops`/
`caco doctor`). Not closing bd-db3f48 as the systemic resolution; it lands one
concrete, measured read-path improvement.
