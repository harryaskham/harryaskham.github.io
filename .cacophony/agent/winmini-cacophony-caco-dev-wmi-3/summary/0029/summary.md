# Pending summary — bd-f147d3

## Bead
- bd-f147d3 — Switch client-node/bootstrap config to Tailnet-direct binds after support lands.

## Changes
- Updated checked-in `.cacophony/config.yaml` bootstrap settings to the Tailnet-direct shape:
  - `pki.bootstrap.public_url`: `https://helsinki.miku-owl.ts.net:18443`
  - `pki.bootstrap.bind_port`: default `18443`
  - `pki.bootstrap.exposure: tailnet_direct`
- Removed temporary `iphone` / `ipad` static daemon-node entries that existed only as pre-first-class client certificate identities.
- Fixed `.cacophony/client_nodes.yaml` to declare top-level `iphone` and `ipad` under `client_nodes` (instead of a nested `client_nodes.client_nodes` map).
- Switched `iphone` and `ipad` command-server binds from wildcard `0.0.0.0` to `tailnet`, preserving their Tailnet hostnames and port `11505`.

## Validation
- `caco config validate --project-config-dir .cacophony`
- `caco --config .cacophony/config.yaml config validate --show-materialized`
- `git diff --check`

## Notes
- The currently installed `caco` binary used for validation is older than the just-landed `pki.bootstrap.exposure` source field, so its materialized-config echo does not serialize that new field yet. The source config contains `exposure: tailnet_direct`, and the source-side validation landed in bd-8d08cc.
