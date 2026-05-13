# Session summary — transcript narrator lifecycle recovery fallback

## Goal

Fix `bd-ee2b6a`, where helsinki's stopped transcript narrator with `resume_blocker=tmux_socket_dead` still produced lifecycle POST transport errors after deployment of the prior timeout work. The session focused on making the first-party lifecycle path responsive and able to request a replacement persistent launch instead of driving the ordinary directory-resume path that was failing before response.

## Bead(s)

- `bd-ee2b6a` — Cannot unpause transcript narrator: lifecycle POST endpoints transport-error

## Before state

- Failing tests: no targeted test covered the `Stopped` persistent + `tmux_socket_dead` resume path.
- Relevant metrics: helsinki daemon was reachable on `/api/v1/node` at commit `f2cb1b8c5`, but `POST /api/v1/agents/helsinki-cacophony-transcript-narrator/resume` returned `daemon_endpoint_transport_error`; direct local curl reproduced an empty reply for the resume/discard class while `/api/v1/node` and read-only status were healthy.
- Context: the narrator remained `stopped` with `resume_blocker=tmux_socket_dead`, a preserved checkout, and persistent id `persistent-473cc192`.

## After state

- Failing tests: none observed in targeted validation.
- Relevant metrics: daemon now has a `tmux_socket_dead` persistent resume fast path: if a terminal persistent backing agent has that blocker, `/agents/{id}/resume` returns an immediate success envelope with `resume_method=persistent_relaunch_requested` and spawns the persistent relaunch in the background. `/persistent/{id}/start` now also kicks the direct persistent launch path rather than only setting a sentinel flag and waiting for periodic reconciliation.
- Context: this preserves responsiveness for lifecycle POST callers while cleanup/bootstrap happens asynchronously; ordinary non-persistent and non-`tmux_socket_dead` resume paths are unchanged.

## Diff summary

- Code/content commits: `9d762aa1f` (`bd-ee2b6a: relaunch tmux-dead persistents`)
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched: `crates/caco-daemon/src/lib.rs`; `.cacophony/agent/winmini-cacophony-caco-dev-wmi-1/summary/pending/summary.md`
- Tests: +1 targeted helper test for the `Stopped` persistent + `TmuxSocketDead` fallback predicate
- Validation: queued `cargo test -p caco-daemon --lib bd_ee2b6a -- --nocapture` passed after rebase as `tj-eb11c591`; queued `cargo test -p caco-daemon --lib agent_resume_path_gets_extended_request_timeout_bd_8ec919 -- --nocapture` passed as `tj-dad3202c`; queued `cargo test -p caco-daemon --lib agent_lifecycle_paths_get_extended_request_timeout_bd_3d141a -- --nocapture` passed as `tj-be8ca7f3`; `git diff --check origin/main..HEAD`.
- Behavioural delta: first-party resume/start of a terminal persistent with dead tmux socket should return a structured response immediately and request replacement launch, rather than leaving operators with a transport error while the daemon health endpoint is green.

## Operator-takeaway

For a persistent observer like transcript-narrator, `tmux_socket_dead` is now treated as “request a replacement persistent launch” rather than “try the brittle directory-resume path”; the operator should get an immediate lifecycle response and the daemon should relaunch in the background.
