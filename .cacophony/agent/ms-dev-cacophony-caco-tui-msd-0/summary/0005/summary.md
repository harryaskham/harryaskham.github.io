# Session summary — self-heal missing in-memory checkout registration at spawn-readiness (bd-50e049)

## Goal

Pitch into a non-gated daemon-Rust bead per Harry's "take capable specialist work, don't sit idle" directive. Fix a spawn-readiness bug the controller routed to me: an agent spawn could fail with a spurious `checkout_not_found` 503 ("canonical checkout path for project '<p>' is not available yet") even when the project's canonical checkout EXISTS on disk — because the project was missing from the CheckoutManager's in-memory `projects` map (registration lagged / never ran on that node).

## Bead(s)

- `bd-50e049` — astra doesn't recognise existence of checkouts (spawn-readiness; root-caused by msd-1 as the in-memory-vs-on-disk registration-lag class, sibling of bd-b8af56).

## Before state

- `CheckoutManager::checkout_path` (crates/caco-daemon/src/checkout.rs) is a pure in-memory accessor returning `None` whenever `inner.projects` has no entry for a project — even if the canonical checkout is fully valid on disk. The agent-create spawn-readiness site (crates/caco-daemon/src/lib.rs) turned that `None` into a `checkout_not_found` 503, refusing the spawn.
- bd-b8af56's `verify_on_default_branch` self-heal only covers an ALREADY-registered project with a stale `initialized` flag; it cannot help the NO-ENTRY case.
- Failing tests: none (the bug needed a registration-lag repro).

## After state

- New `CheckoutManager::ensure_project_registered_from_disk(project, config)`: when a configured project is absent from the map, it builds the project state via the existing `build_project_checkout_state` and registers it ONLY when `canonical_checkout_ready` verifies a real checkout (.git, non-placeholder, origin remote, HEAD) — the same VERIFIED tolerance bd-b8af56 uses, so it cannot admit a placeholder/missing/pending checkout. Idempotent no-op when already registered.
- Called at the agent-create spawn-readiness site just before the `checkout_path` check (off the hot accessor). A genuinely-absent on-disk checkout still yields the existing retryable 503.
- Failing tests: none. 3 new unit tests (see Diff summary).

## Diff summary

- Code commit: `7800fe05b6` (agent branch; final landed squash SHA from the reintegration receipt).
- Files touched: `crates/caco-daemon/src/checkout.rs` (new pub method + 3 tests), `crates/caco-daemon/src/lib.rs` (self-heal call at the spawn-readiness site).
- Tests: +3 (`ensure_project_registered_from_disk_noop_when_already_registered_bd_50e049`, `..._registers_verified_on_disk_checkout_bd_50e049`, `..._refuses_when_no_ready_checkout_bd_50e049`), validated via the daemon test queue (`cargo test -p caco-daemon --lib ensure_project_registered_from_disk`).
- Behavioural delta: additive self-heal on the spawn-readiness path; no change to the hot `checkout_path` accessor, no public-API removal. A spawn against an on-disk-ready-but-unregistered project now succeeds instead of 503-ing; a genuinely-missing checkout still 503s retryably.

## Operator-takeaway

Spawns no longer spuriously 503 against a valid on-disk canonical checkout just because in-memory registration lagged on that node — the daemon now verifies on-disk truth and self-registers, mirroring bd-b8af56's verified self-heal but for the never-registered case. Caution honored: the registration is gated on `canonical_checkout_ready`, so it cannot admit a placeholder/missing checkout, and a genuinely-absent checkout still returns the retryable 503 (the real gap there is checkout materialization, not recognition).
