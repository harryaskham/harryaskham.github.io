# Session summary — F2a: TOFU SSH host-key foundation (bd-a1a358)

## Goal
Foundation for replacing the AcceptAll host-key MVP: trust-on-first-use host-key
decision + verifier + known-hosts store (the security hardening the picker will use).

## Bead(s)
- bd-a1a358 F2 / S5-hostkey (SSH terminal host-key verification; follows the landed S5 terminal).

## Before/After state
- Before: newSshClient only implements AcceptAll (insecure); AcceptNew/Strict unwired. Failing: none.
- After: SshKnownHosts.kt — SshTofuDecision + sshKnownHostKey (pure) + sshTofuDecision (TOFU:
  first-use accept+store when allowed, match, reject-on-change/unknown) + TofuHostKeyVerifier
  (sshj HostKeyVerifier using SecurityUtils.getFingerprint) + SshKnownHostsStore (prefs).
  SshKnownHostsTest 5/5 green; compileDebugKotlin validates the sshj verifier API. Failing: none.

## Diff summary
- Code/content commits: pending reintegration receipt SHA.
- Files: connection/SshKnownHosts.kt (new), test SshKnownHostsTest.kt (new).
- Tests: +5, -0. Behavioural delta: none yet (foundation; F2b wires it into newSshClient + the picker).

## Embedded artefacts
- None. Pure TOFU logic + verifier, unit-tested.

## Operator-takeaway
TOFU host-key decision/verifier/store is in + tested. F2b wires it into newSshClient/openSshShell
+ the rememberTermuxAgentTerminalState picker (defaultSshHostKeyPolicy=AcceptNew instead of
AcceptAll), then render-validates the connect with TOFU. F1 (in-app Settings SSH config) is the
other S5 follow-up. The SSH terminal itself is landed + render-validated.
