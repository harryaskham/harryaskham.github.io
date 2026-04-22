# Session summary — bd-efe17d: stop caco doctor false-positive cert errors on secondaries

## Goal

Stop `caco doctor` on a non-authority node from emitting `cert missing`
errors and "run `caco cert issue --node <peer>`" hints for every peer's
cert/key, since that material only ever lives on the PKI authority.
Operator-visible: ms-mac, winmini, beelink etc. should report only their
own cert + the CA, not N−1 phantom errors.

## Bead(s)

- `bd-efe17d` — caco doctor reports false-positive 'cert missing' for
  peer nodes on non-authority hosts

## Before state

- `caco_cert::status(paths, config)` iterates every `config.nodes` entry
  and checks for `<node>.crt` / `<node>.key` on local disk. Correct on
  the authority (helsinki); on every other host it produces N−1
  spurious `missing` `NodeCertStatus` entries per pass.
- `caco-cli`'s doctor renderer called `caco_cert::status` directly and
  pushed an `error` `DoctorCheck` plus a `caco cert issue --node <peer>`
  hint per spurious row.
- Existing tests covered the all-nodes view but not the local-node view.

## After state

- New `caco_cert::status_for_node(paths, config, current_node)` —
  authority-aware variant of `status`. On the authority node it behaves
  identically to `status`. On a secondary it gates the per-node loop to
  `current_node` only. CA cert + `ca_key_present` continue to be
  reported.
- `status` is kept as the all-nodes view so `caco cert status` and any
  other authority-side caller are untouched.
- `caco-cli` doctor renderer now calls `status_for_node(&paths, &config,
  &node_name)`. On a secondary host doctor reports CA + own node cert
  only. On the authority behaviour is unchanged.
- Failing tests: none. New tests:
  - `status_for_node_on_authority_matches_status`
  - `status_for_node_on_secondary_skips_peer_certs` (reproducer for the
    false-positive class)
- `cargo clippy -p caco-cert -p caco-cli --tests` clean.

## Diff summary

- Commits: `c2554753`
- Files touched:
  - `crates/caco-cert/src/lib.rs` — split `status` into
    `status` (all-nodes) + `status_for_node` (local-node) wrapping a
    private `status_inner`. Added 2 unit tests.
  - `crates/caco-cli/src/lib.rs` — doctor renderer switches to
    `caco_cert::status_for_node(&paths, &config, &node_name)`.
- Tests: +2 caco-cert unit tests; 0 removed; 0 flipped.
- Behavioural delta: `caco doctor` on non-authority hosts no longer
  surfaces phantom peer cert/key errors.

## Operator-takeaway

`caco doctor` on ms-mac/winmini/beelink will now show CA + own node
cert only — no more N−1 spurious `cert missing` errors instructing the
operator to run `caco cert issue --node <peer>` from a host that has no
business holding peer cert material. Authority-side behaviour
(helsinki) and `caco cert status` are deliberately unchanged. If a
future renderer wants the all-nodes view it should keep calling
`status`; doctor-style "what does *this* host look like" callers
should call `status_for_node`.
