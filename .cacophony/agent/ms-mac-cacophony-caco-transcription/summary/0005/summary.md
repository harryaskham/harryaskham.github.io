# Session summary — sgu24 STT daemon PID-loop fix

## Goal

Finish the remaining `bd-930017` caco-stt-daemon wiring gap after the earlier ms-mac CoreAudio capture fix: make the configured sgu24 ambient STT daemon stop flapping on stale/self PID detection and stay alive under lifecycle supervision.

## Bead(s)

- `bd-930017` — Make configured ms-mac and sgu24 STT daemons produce transcripts

## Before state

- Failing tests: no code tests were failing for this checkout at the start of this slice; runtime health was failing.
- Relevant metrics: `caco @sgu24 stt daemon status --instance sgu24 --json` reported `process_alive=false`, missing control-port file, and no transcript JSONL. `caco ps` on sgu24 reported `caco-stt-daemon.sgu24` as `unhealthy` with repeated log lines like `refusing to start caco-stt-daemon instance 'sgu24' — another instance is already running with pid ...`.
- Context: sgu24 config had converged enough to include `nodes[].services.caco-stt-daemon`, but lifecycle spawn/status still failed because PID files could point at live-but-dead/zombie or self-written child PIDs on Android/nix-on-droid.

## After state

- Failing tests: none in focused validation.
- Relevant metrics: queued focused validation passed: `tj-ae88b892` (`cargo test -p caco-cli process_liveness_tests --lib -- --test-threads=2 && cargo test -p caco-sidecar start_service_does_not_prewrite_tts_pid_before_child_guard_bd_344478 --lib -- --test-threads=2`), earlier combined check passed as `tj-a92a8798` (`cargo check -p caco-cli --tests && cargo check -p caco-sidecar --tests`), and `git diff --check` plus direct rustfmt checks passed. A bounded live sgu24 workaround using the first-party `caco stt daemon` command produced `process_alive=true`, `capture_running=true`, `muted=false`, and `caco ps` showed `caco-stt-daemon.sgu24` running.
- Context: both configured daemon instances were alive when checked: ms-mac capture running with `last_error=null`, and sgu24 capture running with `last_error=null`. The transcript buffers still had zero entries at that exact check, so further ambient speech may be needed to verify actual STT segment emission after the daemon remains up.

## Diff summary

- Commits: `04c6226da`.
- Files touched: `crates/caco-cli/src/audio_cmd.rs`, `crates/caco-sidecar/src/lifecycle.rs`.
- Tests: +3 focused regression tests / -0 / flipped 0.
- Behavioural delta: STT live/daemon PID liveness now treats zombie processes as dead on Linux/Android and macOS, matching sidecar lifecycle semantics. Lifecycle no longer parent-writes fallback PID files for self-guarded PID-only daemons such as TTS/STT after a short grace, preventing slower Android child startup from reading its own parent-written PID and exiting as a duplicate instance.

## Operator-takeaway

The remaining sgu24 failure was a durable lifecycle race, not missing config: the supervisor could make the STT child collide with its own singleton guard. The fix makes lifecycle wait for the child-owned PID file instead of manufacturing one, and sgu24 ambient STT is now able to stay running with capture active.
