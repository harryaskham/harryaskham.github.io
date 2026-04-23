# Session summary — Codespaces user-facing docs (bd-2ba632)

## Goal

Translate the two design docs (bd-1975be architecture and
bd-f32dda key distribution) into a single user-facing guide
operators can read top-to-bottom to provision and use a
GitHub Codespace as a Cacophony node.

## Bead(s)

- `bd-2ba632` — Document Codespaces node setup and usage
  (P2, codespaces, documentation)

## Before state

- Two design docs landed under `docs/epics/` (bd-1975be,
  bd-f32dda) — comprehensive but engineering-facing (threat
  model tables, daemon endpoint contracts, etc.).
- No top-level operator-facing guide. An operator wanting to
  spin up a codespace had to read both designs and infer the
  CLI shape themselves.

## After state

- New `docs/codespaces.md` (~12.4KB) covers:
  - **What you get** + when to use codespaces vs physical
    nodes (real-time audio / persistent gaps / GPU / docker
    builds = use physical; clean room / shareable / burst /
    mobile-friendly = use codespace)
  - **Prerequisites** on operator host (gh CLI, repo clone
    with devcontainer, caco install, rendezvous node) and
    on GitHub (codespaces enabled, repo write access)
  - **Setup happy path** — 5 commands from `codespace new` to
    `attach`
  - **Under-the-hood explanation** of `caco codespace new`
    pulled directly from bd-f32dda §5
  - **Per-task secrets** with all four source backends
    (`--from-keyring`, `--from-file`, `--from-sops`,
    `--from-key-vault` referencing bd-6d16a7's
    `deploy/aca/SECRETS.md`)
  - **Lifecycle commands table** — ls/resume/stop/remove/
    rekey/revoke
  - **Troubleshooting** for the 5 expected failure modes from
    bd-f32dda §9: enrollment_expired, pubkey_mismatch,
    rendezvous unreachable, suspended-but-listed, agent
    spawn failure
  - **Security considerations** with 6 headlines pulled from
    bd-f32dda's threat model — operator-host master keys
    never enter codespace; codespace-local ed25519; per-task
    secrets opt-in; single-use 10-min token; tofu pinning;
    GitHub trust assumptions
  - **Cost implications** — auto-suspend after 30min idle,
    no warm-keep traffic from rendezvous, ~$0.07/GB/month
    storage, wake latency 10-30s
  - **Common usage patterns** — burst parallelism (4-codespace
    fan-out), mobile/phone-friendly, shared session with
    teammate, throwaway experiment
  - **What's NOT supported (yet)** — cross-org pools, SSE
    push, GPU, persistent state across remove,
    auto-rotation (with Key Vault exception noted)
  - **See-also section** linking back to the two source
    designs and SECRETS.md

## Diff summary

- Files touched:
  - `docs/codespaces.md` (new)
- Tests: +0 / -0 (docs bead, no code)

## Operator-takeaway

Single-doc entry point for the Codespaces feature. Operators
who want depth go to `docs/epics/bd-1975be-…` and
`docs/epics/bd-f32dda-…`; everyone else reads `docs/codespaces.md`
and is productive in 5 commands.

Honored constraints:
- Operator note "no docker builds locally; use Azure" surfaced
  in "When to use codespace vs physical node" (codespaces are
  NOT for docker builds; physical via Azure).
- Operator note "narrator stopped; agents narrate own progress"
  honored — claim + close speaks issued by msm-2 directly.
- caco-ctrl note about Key Vault unblock (bd-6d16a7) folded
  into the per-task secrets section as the preferred backend
  for cloud-deployed codespaces.

This is the fourth artefact this session in the codespaces
lane:
1. bd-1975be design (~13KB)
2. bd-f32dda key-distribution design + threat model (~17.5KB)
3. **bd-2ba632 user-facing docs (~12.4KB)** ← this bead
4. (leaves bd-d8c118 / bd-c38698 / bd-dce8ed / bd-0bed93 /
   bd-869dca / bd-77653d for the implementation pass)

The implementation beads now have BOTH a wire-contract spec
AND a user-facing guide pinning the CLI surface they must
ship. CLI shapes used in `docs/codespaces.md` (e.g.
`caco codespace secret push --from-key-vault <url>`) become
acceptance criteria for the implementation beads.
