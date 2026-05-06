# Session summary — tmux fallback for caco-aks resume

## Goal

Restore the failed persistent caco-aks agent on ms-mac and prevent recurrence of its post-outage `tmux_creation_failed` resume blocker, without taking unrelated release, runner, or local ms-mac repair work.

## Bead(s)

- `bd-2877e9` — Persistent caco-aks cannot resume on ms-mac: tmux_creation_failed

## Before state

- Failing surface: `caco agent status --id ms-mac-cacophony-caco-aks` reported `state=failed`, `resume_blocker=tmux_creation_failed`, and `tmux new-session failed to launch ... No such file or directory`.
- Context: ms-mac was recovering from the `bd-dcafee` outage. Interactive remote checks found `tmux` at `/Users/harryaskham/.nix-profile/bin/tmux`, but daemon-supervised resume could not find it.

## After state

- Runtime evidence: a first-party `caco agent resume --id ms-mac-cacophony-caco-aks` initially timed out while the provider became ready, then `caco agent status` showed `state=running` with fresh activity; `caco @ms-mac ps` listed `ms-mac-cacophony-caco-aks` running in its preserved checkout/session.
- Validation: queued `caco test run` job `tj-d1019981` passed `cargo test -p caco-daemon resolve_tmux_binary --lib` with all 3 new tests passing. `cargo fmt --all -- --check` passed locally as a source-only formatting check.
- Caveat: a shell quoting mistake while drafting this summary attempted to execute Markdown backtick content locally; I stopped, checked for stray matching cargo processes, and rewrote the summary with the file tool. The durable validation evidence remains the queued test job above.

## Diff summary

- Commits: `4ac846867` (code change; this summary is in the following summary commit)
- Files touched: `crates/caco-daemon/src/agent/health.rs`
- Tests: +3 unit tests for tmux binary resolution.
- Behavioural delta: tmux helpers now resolve `tmux` through PATH first and then managed fallback locations such as `~/.nix-profile/bin`, `/nix/var/nix/profiles/default/bin`, Homebrew, and system paths before falling back to bare `tmux`. Tmux diagnostics/cleanup helpers share that resolver instead of using raw `Command::new("tmux")`.

## Operator-takeaway

The caco-aks failure was recoverable and is now running; the durable fix is to make daemon-spawned tmux commands tolerate supervisor PATH drift on macOS/Nix-managed hosts rather than depending on the narrower launchd environment.
