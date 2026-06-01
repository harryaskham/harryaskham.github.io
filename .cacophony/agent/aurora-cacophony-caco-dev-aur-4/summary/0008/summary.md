# Session summary — bd-5ef3ea: concurrent + timeout-bounded canonical-checkout refresh

## Goal

Stop the daemon's canonical-checkout refresh loop from being serial and
unbounded, which let canonical mirrors fall fleet-wide stale far past the 60s
cadence and caused repeated stale-mirror coordination mix-ups (agents reading a
behind daemon-local mirror as true GitHub main). This is the dev-fixable root
MECHANISM behind the stale-canonical-mirror friction (bd-cd40dc) and the same
structural anti-pattern class as the bd-0282ac peer-probe wedge: one slow
blocking op freezes a serialized batch.

## Bead(s)

- `bd-5ef3ea` — Canonical-checkout refresh loop is synchronous + serial +
  unbounded: one slow git fetch wedges all-project freshness. P1 bug,
  broken-on-main/checkout-isolation/concurrency/reliability/replication.
- Filed `bd-b0c801` — [broken-on-main] two pre-existing checkout::tests failures
  (reconcile_config hot-reload + sparse_spec_change_triggers_regenerate),
  unrelated to this change.
- Cross-referenced `bd-b1ce01` (P0) — the silent-swallow escalation/recovery half
  is deliberately left to it (coordinated with aur-2); this slice is the
  structural wedge only.

## Before state

- `refresh_all` (checkout.rs) was a plain serial `for name in names { refresh }`
  loop: total pass latency = SUM of every project's fetch. With 18+ projects at
  ~2-4s each, the healthy-case pass already meets/exceeds the 60s tick.
- `refresh` -> `fetch_and_reset` -> `run_git` ran `git fetch` with NO timeout, so
  one slow/hung fetch (SSH-over-443 stall, auth prompt, network blip) blocked the
  task indefinitely and wedged the whole loop.
- Failing tests: none in the refresh area (2 pre-existing unrelated checkout
  failures exist, now bd-b0c801).

## After state

- Failing tests: none from this change. New
  `refresh_all_concurrent_chunks_preserve_all_projects_bd_5ef3ea` passes;
  existing `refresh_all_defers_uninitialized_projects_bd_83862c` still passes;
  clippy `-p caco-daemon --lib` clean.
- `refresh_all` refreshes projects CONCURRENTLY in bounded chunks
  (`REFRESH_CONCURRENCY = 6`) via `futures_util::future::join_all`, so pass
  latency is ~the slowest single fetch, not the sum. Safe because
  `with_project_state` clones state and releases the manager lock before the
  per-project fetch closure (bd-83d9aa).
- The refresh fetch is timeout-bounded: new `run_git_fetch_with_timeout`
  (reusing `git_command_with_timeout`'s process-group-kill machinery) with
  `REFRESH_FETCH_TIMEOUT = 45s`, applied to both the primary fetch and the
  SSH-over-443 fallback fetch. A hung fetch now fails for this cycle with a
  distinct timeout error instead of wedging the loop.

## Diff summary

- Code commit: pending final squash SHA from the reintegration receipt.
- Files touched: crates/caco-daemon/src/checkout.rs (concurrency + timeout +
  2 consts + 1 helper + 1 test).
- Tests: +1 (concurrent-chunk accounting).
- Behavioural delta: refresh pass is concurrent and each fetch is hard-bounded;
  one slow/hung project no longer stalls the others or the loop. The
  per-project `eprintln` surfacing is intentionally unchanged.

## Embedded artefacts

- none.

## Operator-takeaway

This is the structural half of the stale-mirror root cause: refresh was serial
(sum-latency over 18+ projects already blew the 60s budget) AND the fetch had no
timeout (one hung dial wedged the loop). Now it's concurrent + bounded, so
mirrors stay within ~one cadence and a single bad remote can't freeze fleet-wide
freshness. The OTHER half — escalating/recovering persistent fetch failures that
`refresh_all` currently swallows to stderr (bd-b30310's finding) — is
deliberately left to bd-b1ce01 so the two don't double-implement the same
surfacing; this slice adds no failure-counter plumbing. The deepest fix (move
ALL blocking git off the request-serving runtime via spawn_blocking to prevent
runtime-worker starvation) remains a broader follow-up noted in the bead.
