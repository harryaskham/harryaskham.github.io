# Summary 0007 — bd-dc9fab caco-web bind retry window extended

## Goal
Stop caco-web giving up on its port-11180 bind during daemon restart while the
prior process is still being reaped by the tts-watchdog (bd-5f8223), which can
take up to ~2.5 minutes. The previous retry schedule only spanned ~30s, so
caco-web exited with `AddrInUse` even though the dashboard would otherwise
have come back healthy a minute later.

## Bead(s)
- bd-dc9fab — caco-web AddrInUse recurred during ms-mac v1.2.594 restart;
  prior bd-60f6db not found after authority migration (P2 bug).

## Before state
- `crates/caco-web/src/server.rs::bind_with_retry` used backoffs
  `[1, 2, 4, 8, 15]` summing to 30s before the final attempt — well below the
  ~150s tts-watchdog reap window.
- Symptom from `daemon-crash.log` 2026-04-29T08:09:05–35Z: 5 retries, then
  `Web dashboard exited: Address already in use (os error 48)`.
- No regression test guarded the total backoff window against the watchdog
  reap window.

## After state
- New module-level constant `BIND_RETRY_BACKOFFS_SECS` in `server.rs` holds
  the schedule `[1, 2, 4, 8, 15, 30, 45, 60]` (sum = 165s) plus a final
  attempt; the doc comment cross-references bd-dc9fab and bd-5f8223.
- `bind_with_retry` reads from the constant, no behavioural drift other than
  the longer schedule.
- New test `tests::bind_retry_backoff_window_covers_watchdog_reap_window`
  asserts `BIND_RETRY_BACKOFFS_SECS.iter().sum() >= 150` so any future
  schedule shrink without a matching watchdog-reap shrink fails CI.
- Existing `bind_with_retry_succeeds_after_port_freed` still passes (it
  exercises the first retry within 1.5s, unaffected by the new tail).

## Diff summary
- `crates/caco-web/src/server.rs`: hoisted backoff schedule to a `pub(crate)
  const`, extended from 5 → 8 entries, doc-commented the rationale.
- `crates/caco-web/src/tests.rs`: added one regression test
  (`bind_retry_backoff_window_covers_watchdog_reap_window`).
- `cargo test -p caco-web --lib bind_`: 2/2 pass.
- `cargo check --workspace --tests`: clean.

## Operator-takeaway
caco-web will now retry the port-11180 bind for ~2¾ minutes instead of ~30s,
which closes the AddrInUse-on-restart gap without operator-visible downtime
for typical daemon restarts. The backoff schedule lives in a constant locked
by a dedicated test, so any future tuning has to keep the watchdog-reap
invariant in mind. bd-60f6db (lost during the authority migration) is
implicitly closed by this fix; bd-3af67d / bd-2324c2 were the prior closed
trackers for the same family.
