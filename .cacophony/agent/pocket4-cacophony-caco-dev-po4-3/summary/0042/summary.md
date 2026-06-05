# Summary — bd-a5b25e cache per-checkout HEAD git probe (beads-read latency under compile load)

## Goal
bd-a5b25e (P2, beads/performance/ms-mac): recurring intermittent beads-read
latency spikes (25-40s) on ms-mac under sustained compile load. `/api/v1/beads`
full-scan + board count_only intermittently stall while adjacent reads stay
sub-second; daemon core healthy, data intact (board count 45 correct), no
DB-lock lines. CLI `bd create` also stalls during spikes; direct API POST is
fast.

## Root cause
`compute_checkout_health()` (crates/caco-daemon/src/beads.rs), called inside the
`beads_status_sweep` for **every project on every** `/api/v1/beads` and
`/api/v1/beads/status` request, shells out to **two git subprocesses**
synchronously: `git rev-parse --short HEAD` and `git log -1 --format=%ct HEAD`.
Under dozens of parallel rustc/cargo processes saturating CPU+IO, those
subprocess spawns intermittently stall 25-40s — explaining the ~1-in-3 deep-read
stall pattern, correlation with compile load, intact data, and absence of
DB-lock lines (the SQL is fine; the cost is per-request process spawns). The CLI
`bd create` pre-write reads hit the same slow window; direct API POSTs that skip
the health sweep return fast.

The SQLite path was profiled and ruled out: `count_by_status` uses
`SELECT status, COUNT(*) ... GROUP BY status` (hits `idx_issues_status`);
`list_ready` is the already-optimized bd-b39bdc lazy-dependency path. At 45 rows
neither is a 25-40s scan.

## Fix
Route the two HEAD probes through a process-global short-TTL cache
(`CHECKOUT_HEAD_PROBE_TTL = 5s`) keyed by checkout path. A burst of reads under
load collapses onto one git spawn per checkout per 5s window instead of one per
request. The **raw HEAD commit timestamp** is cached (not the derived age), so
`head_age_secs` is recomputed from the current time on every read and stays
accurate-to-the-second — only the expensive subprocess spawns are memoized. No
`DaemonState` field churn: the cache is a `LazyLock<Mutex<HashMap>>` local to the
beads module, correct because HEAD is a property of the on-disk path, not of a
`DaemonState` instance (avoids editing ~10 struct-init sites).

## Test
`cached_checkout_head_caches_head_probe_bd_a5b25e`: builds a temp git repo,
asserts the first cached probe returns the real short HEAD + a fresh age, that
the probe is then cached for that path, and that a repeat read within the TTL
reuses the cached `head_ref` while still deriving a fresh age. Green via the
16MB-stack daemon lib lane; caco-daemon lib compiles clean.

## Scope / remaining
Targets suggestion #1/#3 (avoid per-call work / isolate beads reads from compile
pressure via a read cache) — the highest-leverage, lowest-risk slice. Deeper
options (nice/ionice for the sweep, fully async git, or moving health out of the
hot read path) remain available if spikes persist after this lands, but the
per-request git-spawn elimination should remove the dominant stall source.
Headless-verifiable (daemon Rust logic, no visual).

## Diff
See the landed squash commit in the reintegration receipt (code commit
`07b91c99c` touching `crates/caco-daemon/src/beads.rs`).
