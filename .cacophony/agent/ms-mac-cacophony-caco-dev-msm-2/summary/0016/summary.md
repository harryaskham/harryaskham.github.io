# Session summary — caco agent new profile preflight (bd-099b6a)

## Goal

Surface "profile typo" mistakes at the CLI BEFORE any daemon round-trip so operators don't have to wait for a post-spawn 'failed' state with a cryptic reason.

## Bead(s)

- `bd-099b6a` — Spawning agents as project-controller via 'caco agent new': no validation that target node accepts the requested profile.

## Before state

- `caco agent new --profile <typo>` POSTs to `/api/v1/projects/<p>/agents`; the daemon rejects with `profile '<typo>' not found` only after attempting checkout work.
- For remote-node spawns where the operator misremembers the profile name, the failure was post-spawn with no actionable suggestion.

## After state

- Client-side preflight when `--profile` is set:
  1. `GET /api/v1/profiles` on the local daemon (best-effort, 2s timeout).
  2. If the requested name is missing, print a stderr warning with up to 3 substring-match suggestions.
  3. Continue with the spawn anyway — the server-side check remains authoritative.
- 3 small helpers extracted for unit testability without a live daemon.
- 6 new unit tests cover envelope parse, silent paths (empty inventory, present profile), and warning paths (with/without suggestions).
- Smoke test on live daemon confirmed the warning fires before the server error.
- `cargo test-small` clean (209/109/739/291/18/2817/56). `cargo check --workspace --tests` clean.

## Diff summary

- Commit: `93e0d3a7`
- Files touched: `crates/caco-cli/src/lib.rs` (+151 / -0).
- Tests: +6 unit; 0 removed; 0 flipped.
- Behavioural delta: opt-in via `--profile`. JSON callers unaffected (warning to stderr).

## Out of scope (filed implicitly for follow-up)

- `POST /api/v1/agents/dry-run` server endpoint (resources, auth, per-node profile inventory).
- Per-target-node profile-list query — currently we check only the LOCAL daemon's view; remote nodes with different daemon binary may embed a different profile set.
- `spawning... [validating target node, OK]` progress UI.

## Operator-takeaway

`caco agent new --profile <typo>` now warns with substring suggestions before the network round-trip. Wider dry-run / multi-aspect validation is still owned by the parent bead.
