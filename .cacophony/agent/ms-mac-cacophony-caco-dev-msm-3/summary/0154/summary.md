# Session summary — TTS spoken-name status source correction

## Goal

Follow up on the reopened `bd-ee9408` after the live ms-mac binary refreshed to 1.2.752 and log-monitor observed a new daemon-beads-all spoken-name lookup failure. The goal was to distinguish remaining transient lookup failures from stale UI-snapshot drift and make the operator-facing TTS status identify the live beads-all source correctly.

## Bead(s)

- `bd-ee9408` — TTS spoken-name refresh times out daemon UI snapshot on ms-mac

## Before state

- Failing tests: none known in source; the live ms-mac TTS daemon had recurrent runtime failures.
- Relevant metrics: after the 1.2.752 refresh, `/Users/harryaskham/.cargo/bin/caco --version` reported `caco 1.2.752 (92f2d607f)`, and `caco tts status --json` showed endpoint `http://127.0.0.1:11100/api/v1/beads/all?limit=5000` but still serialized `source=daemon_ui_snapshot`; latest failure was a `daemon_beads_all` request error at `2026-05-07T22:50:34Z`.
- Context: the mainline code had already moved spoken-name refresh off `/api/v1/ui/snapshot`; the remaining confusion was status metadata retaining the old source label despite the new endpoint.

## After state

- Failing tests: none in focused queued validation.
- Relevant metrics: queued `tj-e9af0c47` passed `tts_status_serializes_runtime_mute_source_bd_fe2d81`; queued `tj-5499ff82` passed `tts_daemon_spoken_name_beads_lookup_reports_503_metadata_then_recovers`; queued `tj-4db9126b` passed `cargo clippy -p caco-cli --lib --no-deps -- -D warnings`.
- Context: `TtsSpokenNameSnapshotStatus::new`, success updates, failure updates, and error construction now use the shared `daemon_beads_all` source constant for the beads-all refresh path.

## Diff summary

- Code/content commits: `b04a6c70e9`.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched: `crates/caco-cli/src/lib.rs`, `.cacophony/agent/ms-mac-cacophony-caco-dev-msm-3/summary/pending/summary.md`.
- Tests: +0 net tests; updated existing status serialization expectation to the new beads-all source.
- Behavioural delta: `caco tts status --json` / `--explain` will no longer report `daemon_ui_snapshot` as the spoken-name source when the live endpoint is the beads-all read model, making future bd-ee9408 verification less ambiguous.

## Operator-takeaway

The refreshed service did move to the beads-all endpoint, but status metadata still carried the old UI-snapshot source label. This patch fixes that observability mismatch; any future `daemon_beads_all` failure should now be clearly distinguishable from the original 60-second UI snapshot timeout path.
