# Session summary — Beelink recreate config-drift guard

## Goal

Fix `bd-351209`, where Beelink agent recreation could be blocked by ordinary config drift even though the target node was reachable and API-compatible.

## Bead

- `bd-351209` — Stop blocking Beelink agent recreation on compatible config drift

## Work performed

- Audited cross-node agent lifecycle forwarding and peer-health/config-mismatch handling.
- Kept the existing rule that a reachable `mismatch` peer does not preempt real daemon API forwarding.
- Removed the historical `has cluster identity/config mismatch` string from `is_peer_unavailable_error(...)` so legacy config-mismatch forwarding diagnostics are no longer converted into `peer_unreachable` lifecycle blockers.
- Updated `forward_failure_response` test coverage so config mismatch maps to `remote_forward_failed` rather than `peer_unreachable`, while true unreachable/degraded peers still map to `503 peer_unreachable`.
- Updated `SPEC.md` peer liveness contract: reachable config mismatch is a warning/diagnostic and must not block cross-node lifecycle forwarding such as agent recreate when the daemon is API-compatible; concrete transport, mTLS, API, and capability failures remain hard blockers.

## Validation

Passed:

- `cargo test -p caco-daemon forward_failure_response_distinguishes_unreachable_peer_from_real_failure -- --test-threads=1`
- `cargo test -p caco-daemon config_mismatch -- --test-threads=1`
- `cargo check -p caco-daemon --lib`
- `rustfmt --edition 2021 --check crates/caco-daemon/src/lib.rs`
- `git diff --check`

## Outcome

Ordinary cross-node config drift no longer surfaces as peer-unreachable for recreate/lifecycle forwarding. Operators still receive explicit forwarding diagnostics for actual failures, and unsafe transport/API/capability failures continue to block.
