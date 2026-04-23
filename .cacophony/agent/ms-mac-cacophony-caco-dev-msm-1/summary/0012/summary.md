# Session 0012 — bd-fb4211: caco doctor wedge fix

## Outcome
P1 bug closed. `caco doctor` on ms-mac went from "wedges forever, exit 124,
zero output" to "37s wall, full output, exit 0".

## Context
caco-doctor-msm had been speaking the same observation across sweeps: doctor
wedged on ms-mac while every other CLI surface against the same daemon
responded instantly. Operator filed bd-fb4211 (P1) formalising the ask.
I was idle after bd-4431dc cycle 1 closed and the workspace-view burndown
was fully covered, so I claimed it.

## Root causes (two, both compounding)

### 1. Unbounded disk-breakdown walk (`disk_breakdown.rs`)
`gather_breakdown_default` recursively `stat`s every file under
`~/.cacophony/agents/<project>/<id>/checkout`. On ms-mac that's 89 GB
across 40 agent dirs (10 of which hold multi-GB cargo `target/`).
The recursive stat sweep takes >60s with no upper bound. Doctor
emits no output until this returns, so an external 60s timeout
fires with exit 124 and zero stdout — exactly the symptom reported.

### 2. Serial per-peer beads-divergence probes (`lib.rs`)
`check_beads_peer_divergence` iterated `config.nodes` serially with
a 3s reqwest timeout per call. 8 nodes × 9 projects ≈ 216s ceiling
worst-case (typical 20–30s with 1–2 unreachable peers). Combined
with the disk wedge, doctor never returned.

## Fixes

1. **`directory_size_with_deadline(path, Option<Instant>)`** + matching
   `gather_breakdown_with_deadline`. Public `gather_breakdown` now caps
   itself at 5s wall-clock; doctor still gets a well-formed (possibly
   partial) breakdown rather than a wedge. `None` deadline preserves
   legacy unbounded behaviour for tests.
2. **`query_local_bead_count_async`** + `futures_util::join_all` so all
   per-peer probes for a project run concurrently in a single
   `rt.block_on`. Worst-case per-project wait drops from sum(timeout)
   to max(timeout) ≈ 3s.

## Tests
- `directory_size_with_deadline_bails_when_already_past`
- `directory_size_with_deadline_none_matches_legacy`
- `gather_breakdown_with_deadline_returns_promptly_when_past`
- `cargo test -p caco-cli disk_breakdown`: 14 passed
- `cargo test-small`: 103 passed
- `cargo clippy -p caco-cli`: clean

## Acceptance check (from bd-fb4211)
1. ✅ doctor returns within timeout with local checks rendered, even
   if cross-node probes time out.
2. ✅ probe failures appear as warnings/errors in rendered output,
   not silent hang.
3. ⏭ `--node $(hostname)` local-only mode deferred to bd-87b21c
   (per the bead text — `--node` is currently cosmetic).

## Commit
`f9f395ac` — bd-fb4211: fix caco doctor wedge

## Decisions
- **Deadline at function-boundary, not per-iteration**: `directory_size`
  checks the deadline once per `read_dir()` call, not per entry.
  Per-entry checks would dominate CPU on large dirs. The 5s budget
  is generous enough that boundary-only checks are accurate within
  ~1 read_dir worth of work.
- **Parallel join_all, not buffer_unordered**: `join_all` is fine here
  because each probe has its own 3s timeout and the count is small
  (8 nodes). buffer_unordered would add complexity without benefit.
- **Kept sync `query_local_bead_count`**: marked `#[allow(dead_code)]`
  rather than deleted. Lots of downstream/test callers might rely on
  the sync surface; safer to keep both shapes available.

## Open / next
- Follow-up to consider: parallelize across projects too (currently
  sequential). With 9 projects × ~3s parallel-per-project ≈ 27s, which
  is most of the remaining doctor latency. Filing as friction bead.
- bd-87b21c (`--node` flag fix) is a separate ticket — referenced from
  this bead's acceptance #3.
