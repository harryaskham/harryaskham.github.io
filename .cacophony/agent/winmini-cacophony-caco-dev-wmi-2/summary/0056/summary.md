# bd-ab684d Pi resume-runtime mis-detection fix

## Bead
- bd-ab684d (CRITICAL P1) — ms-dev managed Pi launch used `--resume <session-dir/file>` instead of `--session <file>`; the resumption TUI autoselected one session and collapsed all ms-dev agents onto shared history (root cause of the Play Store rollout collision + duplicate Android builds).

## Root cause
- `crates/caco-daemon/src/agent/spawn.rs::detect_resume_runtime` matched an unquoted `exec_line.contains("claude")` / `contains("codex")` fallback in addition to the quoted runtime token.
- A managed Pi exec line that carries a claude/codex model name (e.g. `--model 'github-copilot/claude-opus-4.8'`) then mis-detected as `claude`, so `build_resume_init_script` routed the Pi resume through the Claude branch `inject_resume_flag` → `--resume '<harvested-pi-jsonl-path>'` instead of Pi's `inject_pi_session_flag` → `--session '<file>'`.
- `harvest_pi_session_path` already returns a `.jsonl` FILE (the bead's observed value was display-truncated to the workdir-encoded subdir), so the harvest source/shape was correct; the defect was purely the runtime mis-detection routing Pi onto the Claude `--resume` flag, which trips Pi's interactive resumption picker on load.

## Fix
- `detect_resume_runtime` now matches only the quoted runtime tokens (`'claude'`, `'codex'`, `'pico'`, `'pi'`), consistent with the function's own documented rationale (quoted-token detection to avoid substring false positives). Removed the unquoted `contains("claude")` / `contains("codex")` fallbacks.

## Tests
- Added `detect_resume_runtime_pi_with_claude_model_is_pi_bd_ab684d` (caco-daemon/src/agent/mod.rs): Pi/pico exec lines with claude/codex model names detect as pi/pico; real `'claude'`/`'codex'` launches still detect correctly; unknown → None.

## Validation
- `cargo test -p caco-daemon detect_resume_runtime --lib -- --test-threads=2` → 7 passed (new regression + existing claude/codex/pi/quoted-form/unknown).
- caco-daemon lib compiled clean.
- rustfmt: only pre-existing drift in two untouched test lines; my hunks are rustfmt-clean.

## Operator follow-up (not code)
- ms-dev agents currently collapsed onto one shared session must be RECREATED (`caco agent recreate`) after this fix lands so each gets its own session. That is an operator/controller cross-node action; I am not auto-recreating agents on another node. bd-ab0779 (Play single-owner guard) remains a worthwhile belt-and-suspenders symptom guard, separate from this root fix.
