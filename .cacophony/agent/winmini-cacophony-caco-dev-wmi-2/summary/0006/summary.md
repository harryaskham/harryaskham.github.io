# Session summary — bd-3e39a0: caco secret get validator + envelope hardening

## Goal

Three issues on `caco secret get`:

1. **Issue 1**: misleading 'secrets file not found' error when
   the cluster uses sops-nix and the legacy caco-yaml backend
   isn't initialised.
2. **Issue 2**: JSON envelope was the only catalogued shape
   without an `ok` field — script consumers had no canonical
   success indicator.
3. **Issue 3**: empty `--path ''` silently accepted; the
   downstream errors then masked the empty-path bug.

## Bead(s)

- `bd-3e39a0` — `caco secret get backend (~/.cacophony/secrets.yaml)
  appears VESTIGIAL on sops-nix-managed clusters; --json envelope is
  the FIRST shape to omit even 'ok' field; empty --path '' silently
  accepted`.

## Before state

- `caco secret get --path bogus` →
  `error: secrets file not found: /home/harry/.cacophony/secrets.yaml`
  (no hint that sops-nix is the live backend on this cluster).
- `caco secret get --path '' ` → same misleading file-not-found
  error masking the real (empty-path) bug.
- `caco secret get --path X --json | jq '.ok'` → `null` (absent).

## After state

`dispatch_secret_get` now:

1. Rejects empty `--path` upfront:
   `error: --path cannot be empty (e.g. --path
   providers.openai.api_key)`.
2. Emits an explicit, sops-nix-aware error when the backend
   file is missing:
   `error: caco-yaml secrets backend not initialized (file:
   /home/harry/.cacophony/secrets.yaml). This cluster has
   sops-nix at /home/harry/.config/sops-nix/secrets — caco
   secret reads the legacy caco-yaml backend, not sops-nix`.
   The sops-nix-hint suffix is added only when the sops-nix
   secrets dir actually exists, so non-sops-nix clusters still
   get a clean primary error.
3. Both the failure-path JSON envelope and the success-path
   JSON envelope now include `ok: false` / `ok: true` as the
   first field — script consumers can use the cluster-standard
   `jq -e '.ok'` idiom.

## Diff summary

- `crates/caco-cli/src/lib.rs`:
  - `dispatch_secret_get`:
    - Added empty-path guard at function entry.
    - Replaced `if !secrets_path.exists()` arm with a richer
      error message that detects sops-nix and adds a
      conditional hint suffix.
    - Failure-path JSON envelope: added `"ok": false`.
    - Success-path JSON envelope: added `"ok": true`.
  - 1 new test:
    `dispatch_secret_get_validates_path_and_emits_ok_in_json` —
    source-greps the dispatcher body for: empty-path error
    wording, the new backend-name + sops-nix hint, and both
    `"ok": true` / `"ok": false` JSON envelope keys.
- `cargo test -p caco-cli --lib -- ...`: pass.
- `cargo test-small`: 162 pass.

## Operator-takeaway

This brings `caco secret get` into envelope-shape parity with
the broader cluster surfaces (every `--json` response now
carries an `ok` field) and gives operators on sops-nix
clusters a clear "this surface is the legacy backend, you
want sops-nix" signal without breaking clusters that *do*
use the caco-yaml backend.

Issue 4 of the bead (deliberate-confirmation flow on
`--reveal`) is unverifiable from a cluster without a populated
secrets.yaml; left for whoever next touches this surface on
a populated cluster.

The bigger question the bead raises — whether `caco secret`
should be hidden from `--help` on sops-nix-managed clusters
or have a `caco secret status` introspection surface — is an
operator/policy decision rather than an implementation bug;
not addressed here. The conditional sops-nix hint gives
operators enough breadcrumb to reach the right backend without
requiring that bigger architectural call.
