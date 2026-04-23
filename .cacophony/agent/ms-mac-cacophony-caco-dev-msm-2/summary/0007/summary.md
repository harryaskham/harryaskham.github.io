# Session summary — Codespaces key distribution design (bd-f32dda)

## Goal

Land the security/key-distribution contract for Codespaces-as-
nodes so the implementation beads (bd-d8c118 `caco codespace
new`, bd-c38698 `caco codespace enroll`) have a stable wire to
build against.

## Bead(s)

- `bd-f32dda` — Create secure key distribution mechanism for
  Codespaces (P0, codespaces, keys, security)

## Before state

- bd-1975be Codespaces architecture had landed but pinned
  auth/key handling at a high level only — no threat model, no
  enrollment flow, no rotation/revocation contract, no per-task
  secret push semantics.
- bd-d8c118 (CLI new) was blocked on bd-f32dda; bd-c38698
  (CLI enroll) had no contract to build against.
- bd-6d16a7 Azure Key Vault module landed mid-write per
  wmi-1's unblock note → folded into the secret-source
  backends.

## After state

- New `docs/epics/bd-f32dda-codespaces-key-distribution.md`
  (~17.5KB, 11 sections) covers:
  - **Goals/non-goals**: codespace generates own keypair
    LOCALLY; mesh master never enters codespace; single-use
    10min enrollment token; tofu pin at rendezvous; rotation
    + revocation per-codespace
  - **Threat model** (§3): 8 in-scope threats with mitigations
    (token interception, pubkey forgery, private-key leak via
    fork, replay, token reuse, etc.); 3 out-of-scope (GH
    infrastructure trust, side-channel timing, traffic
    analysis); explicit adversary capability assumptions
  - **Key material inventory** (§4): operator host /
    codespace / rendezvous, with mode bits and never-leave-
    boundary annotations
  - **Enrollment flow** (§5): ASCII sequence diagram, 8 steps,
    TLS-pinned WSS, GH user-secret as the token transport
  - **Per-task secret push** (§6): explicit operator opt-in,
    zero secrets by default, four source backends
    (`--from-keyring`, `--from-file`, `--from-sops`,
    `--from-key-vault` per bd-6d16a7 deploy/aca/SECRETS.md),
    Key Vault preferred for cloud-deployed codespaces
  - **Rotation + revocation** (§7): rekey atomically signs new
    key with old key; revocation tombstone propagates to mesh
    in one rendezvous-poll cycle (~30s)
  - **Storage** (§8): full path / owner / mode / contents
    table for all key material across the three roles
  - **Failure modes** (§9): 6 failure scenarios with detection
    + recovery
  - **Implementation contract** (§10): 4 daemon endpoints
    (`/mesh/enrollment-tokens`, `/mesh/enroll`,
    `/mesh/peers/<id>/rekey`, `DELETE /mesh/peers/<id>`); 6
    CLI commands; bootstrap script contract for the
    devcontainer
  - Acceptance map ticking each bd-f32dda requirement

## Diff summary

- Files touched:
  - `docs/epics/bd-f32dda-codespaces-key-distribution.md` (new)
- Tests: +0 / -0 / flipped 0 (design doc, no code)

## Operator-takeaway

Read before claiming bd-d8c118 or bd-c38698. Five key security
decisions to review:

1. **Codespace generates its own keypair LOCALLY** — operator's
   mesh master keys never leave the host. Codespace fork = at
   most that codespace's identity is exposed; the rest of the
   mesh is untouched.
2. **Single-use 10min enrollment token via GH user-secret** —
   bounds the leak window to seconds (operator push → daemon
   join consume); single-use means replay is impossible. Token
   value never persisted on operator host (only its hash, for
   bookkeeping).
3. **Tofu pin at rendezvous** — first connection publishes the
   codespace's pubkey; every subsequent connection must match.
   Mismatch = hard reject + operator alert.
4. **Per-task secrets opt-in only** — zero secrets by default.
   Operator pushes named secrets per-codespace via
   `caco codespace secret push <hash> <name> --from-key-vault
   <url>`. Codespace removal removes the secrets.
5. **Key Vault preferred for cloud** — bd-6d16a7's Azure Key
   Vault module slots in as the `--from-key-vault` backend;
   gives operators rotation + audit at the Key Vault layer.

If caco-ctrl wants to flip any of these, this doc + the
threat model in §3 are the cheap places to do it before
implementation lands.

This is the third design doc this session (after bd-1975be
Codespaces architecture and bd-db60a1 timeline view). All three
follow the same pattern: 11 sections, threat-or-trade-off
matrix, acceptance map at end. Pattern is solid; future design
beads should adopt.

## Honored constraints

- Operator note: "no docker builds locally; use Azure" — no
  docker invocations introduced; design defers Azure container
  flow to bd-44acfb.
- Operator note: "STT polish is separate workstream" —
  unrelated to this bead.
