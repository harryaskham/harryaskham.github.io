# Session summary — F2b: wire TOFU host-key into the SSH terminal (bd-a1a358)

## Goal
Replace the AcceptAll host-key MVP with TOFU verification in the live SSH terminal:
wire the F2a TofuHostKeyVerifier into newSshClient/openSshShell + the terminal picker.

## Bead(s)
- bd-a1a358 F2b / S5-hostkey (completes the host-key hardening on the landed S5 terminal).

## Before/After state
- Before: the picker used AcceptAll (PromiscuousVerifier — accepts any host key, MITM-able).
- After: newSshClient wires the verifier per policy (AcceptAll->Promiscuous; AcceptNew->TOFU
  first-use-pin; Strict->TOFU no-first-use); openSshShell threads a SshKnownHostsStore; the
  rememberTermuxAgentTerminalState picker + debug harness use defaultSshHostKeyPolicy()
  (AcceptNew) + a prefs-backed store. RENDER-VALIDATED on emulator-5554: TOFU connect to
  ms-dev sshd succeeds (status connecting->live, login banner + harryaskham@ms-dev prompt).

## Diff summary
- Code/content commits: pending reintegration receipt SHA.
- Files: connection/SshClientFactory.kt (TOFU verifier wiring + knownHosts param),
  ui/terminal/TermuxAgentTerminal.kt (controller knownHosts + picker AcceptNew + store),
  src/debug/SshTerminalDebugActivity.kt (TOFU + store).
- Tests: TOFU decision unit-tested in F2a (SshKnownHostsTest 5/5). Behavioural delta: the SSH
  terminal now pins host keys on first use + rejects changes, instead of accepting any.

## Embedded artefacts
- Render-validation screenshot (md21-f2-shot): TOFU SSH terminal on emulator -> ms-dev login
  banner + prompt.

## Operator-takeaway
The SSH terminal host-key policy is now TOFU (AcceptNew), render-proven — the AcceptAll security
gap is closed. Remaining S5 follow-up: F1 (in-app Settings UI to configure the SSH target/key;
currently config-via-prefs). The SSH terminal feature is complete + secure-by-default.
