# Session summary — caco doctor: transient peer-probe demotion

## Goal

Stop `caco doctor` from flipping overall mesh status to `error` on
tailscale-routed peers when the daemon's peer-probe loop drops a
single probe during a DERP path renegotiation. The previous
behaviour drowned genuine outage signals (e.g. truly-offline pocket4
/ astra) in transient blip noise on every doctor run.

## Bead(s)

- `bd-3b7a37` — caco doctor peer probes oscillate
  unreachable<->mismatch on tailscale-routed nodes while
  daemon_state stays fresh

## Before state

- Failing tests: none (behaviour bug, no regression test).
- `dispatch_doctor` mapped peer status -> severity inline. Any
  `unreachable` status went straight to `error` regardless of
  `last_seen` freshness, so a single dropped probe flipped doctor
  to unhealthy even when replication had been touching the peer
  ~10s earlier.
- Operator was running `caco doctor` and `caco @<peer> doctor`
  every 30s and seeing peers oscillate `mismatch` ↔ `unreachable`
  with no real outage.

## After state

- Failing tests: none.
- New `classify_peer_doctor_status` helper consolidates the peer
  severity decision and demotes `unreachable` to `warning` when
  `last_seen_age_secs <= 30` (= 2× `PEER_PROBE_INTERVAL_SECS`). A
  distinct hint marks the demotion: "probe failed but daemon_state
  is fresh (Ns ago) — likely transient (Tailscale renegotiation)."
- Genuine outages (`last_seen` stale or missing) still surface as
  `error` with the original phase suffix.
- Three new unit tests in `caco-cli` lock the contract:
  `classify_peer_doctor_status_demotes_transient_unreachable_to_warning`,
  `classify_peer_doctor_status_keeps_genuine_outage_as_error`,
  `parse_peer_last_seen_age_handles_iso8601_and_garbage`.

## Diff summary

- Commits: `eae634a1`
- Files touched: `crates/caco-cli/src/lib.rs` (+147 / -26)
- Tests: +3
- Behavioural delta: doctor peer status now considers freshness of
  the daemon's last successful probe; transient drops degrade to
  warning, real outages keep error severity.

## Operator-takeaway

`caco doctor` overall status will no longer flip unhealthy on a
single tailscale path-renegotiation drop; persistent unreachables
(stale `last_seen`) still error as before. Threshold lives in
`DOCTOR_PEER_FRESH_LAST_SEEN_SECS = 30s`; tune by editing that
constant alongside any change to `PEER_PROBE_INTERVAL_SECS`.
