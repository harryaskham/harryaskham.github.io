# Session summary — Doctor TTS runtime unmute authorization

## Goal

Repair the first-party path for operator/doctor recovery when local headless TTS is healthy but muted at daemon runtime scope, without granting ordinary workers authority to silence or retune node audio.

## Bead(s)

- `bd-ce2dc8` — Allow first-party repair of local runtime TTS mute from doctor/operator path

## Before state

- Failing tests: no code test failure known; live symptom was `caco tts unmute` / `toggle-mute` from a doctor/cluster-controller profile failing with `cluster_controller scope cannot access /api/v1/tts/unmute`.
- Relevant metrics: daemon scope gate did not recognize `/api/v1/tts/*` runtime control proxy endpoints, so agent-scoped callers reached the default deny path even for cluster-controller repair.
- Context: node/operator bearer callers already bypassed the scope gate, but the intended first-party doctor/operator monitoring profile could not clear local runtime mute through the daemon proxy.

## After state

- Failing tests: none in focused validation.
- Relevant metrics: queued test `tj-0f99b353` passed `RUST_MIN_STACK=33554432 CARGO_BUILD_JOBS=2 cargo test -p caco-daemon --lib tts_runtime_control_scope_allows_doctor_repair_bd_ce2dc8 -- --test-threads=1`.
- Context: `/api/v1/tts/status`-style reads are allowed for agent scopes, while `/api/v1/tts/unmute` and `/api/v1/tts/toggle-mute` mutations are allowed for `cluster_controller` and denied with clear operator/doctor guidance for worker/project-controller scopes.

## Diff summary

- Code/content commits: `5a12f7e581`.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched: `crates/caco-daemon/src/lib.rs`, `SPEC.md`, `.cacophony/agent/ms-dev-cacophony-caco-dev-msd-3/summary/pending/summary.md`.
- Tests: +1 daemon auth-scope unit test; focused queued validation passed. `rustfmt --edition 2021 --check crates/caco-daemon/src/lib.rs` was attempted but reports pre-existing formatting diffs in unrelated module files because rustfmt traverses crate-root modules; no unrelated formatting changes were committed.
- Behavioural delta: doctor/cluster-controller profiles can now use the daemon TTS proxy to repair local runtime mute, while lower agent scopes receive actionable denial copy instead of an opaque default `cannot access` error.

## Operator-takeaway

The next time doctor monitoring finds a node's TTS daemon healthy but runtime-muted, the cluster-controller repair path can unmute it through first-party APIs instead of requiring raw local operator credentials or ad hoc config edits.
