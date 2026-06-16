# Session summary — S3b foundation: sshj dependency + client (bd-a1a358)

## Goal

De-risk and lay the S3b foundation for the SSH-pool terminal transport: add the
SSH client library and prove it's viable on Android — the key open question was
whether sshj resolves/compiles/DEXes without a BouncyCastle conflict (a known
Android risk). Now unblocked: the ms-dev emulator works for the runtime connect
follow-up (ms-dev-2's is node-specifically wedged, bd-ada4b3).

## Bead(s)

- bd-a1a358 S3b foundation (SSH-pool terminal transport epic; follows
  S1/S2/S3a/S5-prep).

## Before state

- Failing tests: none. No SSH client library; S3b feasibility (Android sshj/BC
  compat) unproven.

## After state

- Failing tests: none. `:app:assembleDebug` BUILD SUCCESSFUL — sshj resolves,
  compiles, and DEXes (dexBuilderDebug/mergeExtDexDebug passed, no Duplicate-class
  / BouncyCastle conflict; APK 21.9 -> 25.8 MB). `SshClientFactoryTest` 1/1 green
  (newSshClient constructs an sshj SSHClient on the JVM).
- Added `com.hierynomus:sshj:0.38.0` + `newSshClient(policy)` wiring the S3a
  host-key policy (PromiscuousVerifier for AcceptAll). No connection opened.

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `app/build.gradle.kts` — add sshj dependency.
  - `connection/SshClientFactory.kt` (new) — `newSshClient(policy)`.
  - test `SshClientFactoryTest.kt` (new) — construction/class-load test.
- Tests: +1, -0, flipped 0.
- Behavioural delta: none yet (dependency + client construction only; not wired).

## Embedded artefacts

- None. assembleDebug dex-pass + JVM unit test validate build viability; the
  actual SSH connect/auth + BC-at-runtime is the next slice on ms-dev's emulator.

## Operator-takeaway

The biggest S3b unknown — Android sshj/BouncyCastle dex compat — is RESOLVED
(assembleDebug dexes clean). With ms-dev's emulator now working, the remaining
S3b runtime connect (real sshd handshake + pty channel, using S1 target/S2 key/
S3a policy/S5-prep pty) is a scoped follow-up. SSH foundation S1/S2/S3a/S5-prep +
this S3b dependency foundation are all landed + validated.
