# Session summary — bd-277119 slice C: gated daemon credential resolver

## Goal

Complete the first-party credential-wrapper resolver (bd-277119): a daemon
function that resolves a named credential wrapper to its decrypted secret (e.g.
the Google Play service-account JSON) in memory, so `caco release refresh` can
authenticate to the Play API (bd-5bad1b 2b-iii) without host-coupled shell
scripts. This slice is the resolver itself; the wiring is bd-5bad1b's final
slice.

## Bead(s)

- `bd-277119` — first-party credential-wrapper resolver (slice C of C; this slice
  completes the resolver and the bead closes after it lands).
- parent context: `bd-5bad1b` — Google Play release tracking (read/surface path
  landed; live refresh wiring 2b-iii consumes this resolver).

## Before state

- Failing tests: none.
- `Config.credentials` + `CredentialWrapper` existed (slices A/B) but nothing
  resolved a wrapper to a secret; the only decrypt path was the CLI shell
  scripts.

## After state

- Failing tests: none. `cargo test -p caco-daemon --lib release_play` = passed
  (tj-32d1881b, exit 0): real compile verified (`Compiling caco-daemon`,
  ~16.5 min), 29 tests incl. all 5 new resolver tests ran + ok.
- `caco-daemon::release_play` gains `resolve_credential(name, &credentials)`
  with pure unit-tested seams `build_sops_extract_argv` +
  `resolve_credential_with_runner` (injected runner) and a gated live
  `run_sops_decrypt` (sops -d --extract + optional ssh-to-age-derived 0600 temp
  `SOPS_AGE_KEY_FILE`), plus an `expand_home` helper.

## Diff summary

- Code commits: bd-277119 slice C; final landed squash SHA from the receipt.
- Files touched: `crates/caco-daemon/src/release_play.rs` (resolver fns +
  imports + 5 unit tests).
- Tests: +5 (argv shape, unknown/missing-field errors, argv assembly + secret
  trim, empty-secret rejection, ~ expansion).
- Behavioural delta: a new daemon API to resolve credential wrappers; no caller
  yet (bd-5bad1b 2b-iii wires it). Secret returned in memory only; errors never
  carry secret bytes.

## Operator-takeaway

The credential resolver is complete and secret-safe: pure orchestration seams
are unit-tested while the live SOPS/ssh-to-age exec is gated by being
unexercised by tests (mirroring the openssl/Play-fetch pattern). bd-277119 can
close. The only remaining work for Google Play tracking is bd-5bad1b 2b-iii:
wiring this resolver + `fetch_play_track` into `caco release refresh` to populate
live Play records.
