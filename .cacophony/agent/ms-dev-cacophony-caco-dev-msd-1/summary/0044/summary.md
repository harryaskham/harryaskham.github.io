# Session summary — TTS daemon PID cleanup race

## Goal

Fix the sgu24 doctor finding where `caco-tts-daemon` could be visibly alive in process/log evidence but reported as pid-only stopped because its PID file disappeared after restart churn.

## Bead(s)

- `bd-c0ead1` — [doctor] sgu24 caco-tts-daemon remains pid-only stopped after supervisor start and restart

## Before state

- Failing tests: no focused regression for TTS daemon PID-file cleanup ownership.
- Relevant metrics: `caco @sgu24 restart --service caco-tts-daemon --skip-update` ended with `caco-tts-daemon actual=Stopped (pid-only)` and diagnostics `no PID file`, while remote `pgrep` still found a live `caco ... tts daemon` process.
- Context: logs showed repeated TTS daemon starts/reconnects, consistent with restart races or duplicate process churn where an exiting process could remove shared PID/port files.

## After state

- Failing tests: none in focused validation.
- Relevant metrics: `cargo test -p caco-cli cleanup_tts_daemon_pid_file_only_removes_current_process_bd_c0ead1 -- --nocapture` passed; `cargo check -p caco-cli --tests` passed.
- Context: TTS daemon cleanup now removes PID/port files only when the PID file still points at the exiting process, preserving a newer live instance's PID file for status/watchdog inspection.

## Diff summary

- Commits: `a074ddb18`
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: added 1 targeted unit test covering mismatched-PID cleanup and owner-PID cleanup.
- Behavioural delta: an old or duplicate TTS daemon process can no longer erase the PID file of a still-running replacement instance on exit, preventing false `actual=Stopped, no PID file` status after supervisor/restart races.

## Operator-takeaway

The sgu24 symptom was not just a dead daemon: live process evidence existed, but lifecycle status lost track of it because PID cleanup was not ownership-checked. The fix makes PID-file cleanup conservative so future restarts keep the live daemon visible to `caco status` and doctor.
