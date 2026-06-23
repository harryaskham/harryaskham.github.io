# bd-54dff7 — caco-macos profile: ms-mac nix-daemon in-memory redhill RESOLVED

## Goal
Capture the session-long ms-mac "eval-cache pins redhill" root mechanism (Harry's
nix-daemon-restart finding) into the shared caco-macos profile, retiring the now-stale
override caveat from bd-a4536d.

## Bead(s)
- bd-54dff7 — caco-macos profile: ms-mac nix-daemon in-memory redhill resolved
- follow-up to bd-a4536d; relates bd-880d4c (eval-cache root cause), bd-2bbb1c/bd-716f8e (queue substituter pin)

## Diff summary
- .cacophony/profiles/caco-macos.md (markdown-only): rewrote the ms-mac Rust/nix validation note. ROOT MECHANISM: the running nix-daemon held redhill in its IN-MEMORY substituter cache from before the host-config cleanup; a clean /etc/nix config + determinate-revert do NOT drop it — only a nix-daemon RESTART re-reads the clean config. Empirically confirmed ms-mac clean post-restart (nix develop $CACO_DEV_DIR#caco-runtime --command true: rc=0, zero redhill/504). Override RETIRED for ms-mac (kept only as a transient fallback for nodes whose live nix-daemon still pins a dead substituter); queued-cargo-avoidance retired; no-real-cargo-gate self-validation rule stays.
- Validated: profile-lifecycle-audit clean (83 profiles); git diff --check clean.
- Final landed squash SHA: see reintegration receipt.

## Operator-takeaway
The missing piece of the multi-hour redhill puzzle: host-config-clean != nix-daemon-clean.
The running nix-daemon caches substituters in-memory, so a retired/dead substituter
persists (hanging builds/evals) until the nix-daemon is RESTARTED — a config clean or
determinate-revert alone does not drop it. Future agents now have the correct diagnostic
(override-less nix that 504s on redhill = stale in-memory daemon -> operator nix-daemon restart).
