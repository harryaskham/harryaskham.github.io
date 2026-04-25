# Session summary — bd-afe8f8 verify prebuilt installer downloads

## Goal

Close the install-time integrity gap discovered during the
`gh-pages-security-audit`: `docs/install.sh` and the
README / index / quickstart instructions told operators to download
a release binary and `chmod +x` / exec it without ever verifying a
checksum, signature, or provenance attestation. Anyone who could
compromise the release pipeline upload or the GitHub release assets
could ship a backdoored caco binary that operators would install one
`curl/bash` away.

## Bead(s)

- `bd-afe8f8` — [docs] Add verification to prebuilt binary installer (P3 bug, security/installer)

## Before state

- `release.yml` packaged the per-arch binary and uploaded it as the
  sole release asset.
- `docs/install.sh` ran `curl` + `mv` + `chmod +x` with no integrity
  check.
- Operator-facing docs (README quick-install, index 60s,
  quickstart) only said "inspect the installer first" — no path for
  verifying the binary at all.

## After state

- `.github/workflows/release.yml`:
  - New "Generate SHA256 checksum (bd-afe8f8)" step emits
    `<artifact>.sha256` next to each binary in canonical
    `sha256sum`-compatible format. Picks `sha256sum` on Linux,
    falls back to `shasum -a 256` on macOS runners.
  - Existing python3 release-uploader (bd-7b4fdd / bd-89615a)
    refactored to publish `(asset, sidecar)` pairs under the same
    `release_id` with the same delete-then-upload-with-retries
    dance, so the sidecar can never drift away from its binary or
    be left behind on a per-arch retry. Race-safety with the
    draft-release create-or-find logic is preserved.
- `docs/install.sh`:
  - Fetches `<artifact>.sha256` from the same release URL, computes
    the local SHA256 (`sha256sum` or `shasum -a 256`), refuses to
    `mv`/`chmod` on mismatch.
  - Useful operator-facing diagnostics for: missing sidecar,
    malformed sidecar, no hashing tool, mismatch (refuses install,
    leaves no executable on disk).
  - `CACO_SKIP_VERIFY=1` escape hatch with a 3-line stderr warning
    for trusted air-gapped mirrors.
- `README.md`, `docs/index.html`, `docs/quickstart.html`: document
  the auto-verification path, the sidecar layout, the manual
  `sha256sum -c` recipe, and the `CACO_SKIP_VERIFY` escape hatch.

## Diff summary

- Commit: 967a6aa46
- Files touched: 5 (`.github/workflows/release.yml` +71/-25,
  `docs/install.sh` +51/-2, `README.md` +9/-0,
  `docs/index.html` +3/-0, `docs/quickstart.html` +1/-1).
- Validation: `python3 ast.parse` on the rewritten release.yml
  python heredoc (asset-pairs uploader) is clean; `yaml.safe_load`
  on the workflow parses; `bash -n docs/install.sh` clean; cargo
  test-small 261/261 pass.

## What this catches / doesn't

Catches: compromised release upload (binary flipped pre-sidecar
publish — would mismatch its own published sidecar), compromised
GitHub release asset (binary or sidecar tampered in transit / at
rest — mismatch detected), corrupted download (network truncation
/ mirror bug — same path).

Does NOT catch: both binary and sidecar replaced atomically by an
attacker who controls the release pipeline. Sigstore / cosign /
GitHub artifact attestations should land as a follow-up bead; the
upgrade path is now in place because the verification surface
exists in install.sh.

## Operator-takeaway

`bash install.sh` now refuses to install a binary whose hash
doesn't match the published sidecar. Existing pre-bd-afe8f8
releases without sidecars remain installable via
`CACO_SKIP_VERIFY=1` with a loud warning until they age out. Manual
verification recipe documented in README and quickstart.
