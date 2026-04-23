# Session summary — Codespaces architecture design (bd-1975be)

## Goal

Land a thorough design document for treating GitHub Codespaces as
first-class Cacophony nodes, so the implementation beads
(bd-d8c118 `caco codespace new` CLI, bd-f32dda key distribution,
bd-44acfb Azure remote build) have a stable contract to build
against.

## Bead(s)

- `bd-1975be` — Design GitHub Codespaces integration architecture

## Before state

- Five sibling beads filed in the codespaces / cloud-deploy lane
  (bd-d8c118, bd-f32dda, bd-44acfb, bd-c2cb8b, bd-8b0dbb,
  bd-6d16a7) but no architectural document tying them together.
- Open questions on identifier scheme, ephemeral-node lifecycle,
  bead authorship continuity across rebuild, secrets boundary.

## After state

- New `docs/epics/bd-1975be-codespaces-architecture.md` covers:
  - Motivation and goals/non-goals
  - Identifier model: `cs-<8-char-hash>` with stable rebuild
    continuity, persisted index, caller-string convention
  - Full lifecycle: create / enroll / stop / resume / remove
  - Authentication: ed25519 + tofu like the rest of the mesh,
    one-shot enrollment token via GitHub Codespaces user secrets
  - Tooling provisioning via devcontainer + bootstrap hook
  - Networking: outbound-only, rendezvous-relay model (same as
    Termux nodes)
  - Data plane: lazy-replicating peer (sparse-checkout class),
    pull-on-demand bead and scratchpad replication
  - Trade-off matrix with mitigations
  - Out-of-scope follow-ups (autoscale, pre-warming, cross-org)
- Acceptance map at the end ticks each bd-1975be requirement.

## Diff summary

- Files touched:
  - `docs/epics/bd-1975be-codespaces-architecture.md` (new, 13KB)
- Tests: +0 / -0 / flipped 0 (design doc, no code)
- Behavioural delta: none; this is a contract document for the
  implementation beads.

## Operator-takeaway

Read before claiming bd-d8c118 / bd-f32dda. Key decisions to
review:

1. **Identifier hash from codespace name (not GH UUID)** — gives
   bead authorship continuity across rebuild, pays the cost of
   collision risk (1e-10 at 100 active codespaces).
2. **Lazy replication, not full mirror** — codespaces are
   ephemeral peer-consult clients, not full mesh members. Beads
   pulled on demand via bd-cef230's `/beads/has/<id>` endpoint.
3. **Secrets via GH user secrets, not sops-nix** — codespaces
   are short-lived enough that operator-driven push per-codespace
   is acceptable. NO long-term keys leak into GH environment.
4. **Rendezvous-relay for inbound** — codespaces sit behind GH
   NAT; mesh peers reach them through helsinki the same way they
   reach Termux. +50-150ms RTT cost, acceptable for one-shot
   work, unsuitable for tight controllers.
5. **30min auto-stop is a feature, not a bug** — persistent
   agents stay on always-on nodes. Codespaces optimise for
   zero-friction ephemeral work (one-shot reproductions, fresh
   sandboxes, parallel burndown spawns).

If caco-ctrl wants to flip any of these decisions, this doc is
the cheap place to do it before implementation lands.
