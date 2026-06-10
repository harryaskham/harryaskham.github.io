# bd-2aa981 — Defense-in-depth: hard-guard the Pi session path from the Claude --resume injector

## Bead
bd-2aa981 (daemon/lifecycle/pi/session, P3 follow-up; filer aurora-aur-3) — bd-9ecfff direction 3. Defense-in-depth on top of wmi-2's bd-ab684d/bd-9ecfff root-cause fix (landed 05dd8d435): `detect_resume_runtime` matched an UNQUOTED `contains("claude")`/`("codex")` fallback, so a managed Pi exec line carrying a claude/codex MODEL name (e.g. `--model 'github-copilot/claude-opus-4.8'`) mis-detected as claude → routed the Pi resume through the Claude `--resume <path>` branch instead of Pi `--session <file>`, collapsing all ms-dev agents onto one shared session. wmi-2 fixed detection to match only the quoted runtime token.

## Change
`build_resume_init_script` (crates/caco-daemon/src/agent/spawn.rs), the `"claude"` arm: before calling `inject_resume_flag` (the Claude `--resume <session-id>` injector), guard the session_id:

```rust
if session_id.contains(".pi-agent") {
    return Err(DaemonError::Other(format!(
        "refusing to pass a Pi session path '{session_id}' to the Claude \
         --resume injector — a Pi session path must go to `--session` (misroute)"
    )));
}
```

A Claude `--resume` value is always an opaque provider session ID, never a filesystem path. A `.pi-agent` path reaching the Claude injector is proof of a Pi-session misroute (the exact ms-dev session-collapse defect). The strict-quoting detection fix prevents the misroute at the source; this structural invariant converts any future regression into a loud structured error instead of a silent node-wide session collapse.

## Scope
- Guard placed on the `"claude"` arm only, as the bead specifies (the observed misroute was Pi→claude). The codex arm could get a symmetric guard as a future low-value extension; not done here to keep the change minimal/scoped.
- The optional basename-aware `'pi'` full-path detection (bead "optional extra") was deliberately skipped — wmi-2 judged managed launches always quote the bare `'pi'` token, so it's theoretical.

## Validation (daemon test queue, --cwd at checkout)
- `cargo test -p caco-daemon --lib bd_2aa981` (tj-db31e809): PASSED — `build_resume_init_script_rejects_pi_session_path_on_claude_resume_bd_2aa981` (a claude exec line + a `.../.pi-agent/sessions/<file>` session id returns Err whose message mentions `--session` and includes the offending path).
- `cargo clippy -p caco-daemon --lib` (tj-6f678410): PASSED, 0 warnings.
- Changed regions rustfmt-clean (skip_children); `git diff --check` clean.

## Diff
See the reintegration receipt for the landed squash SHA.
