// Microphone robustness during a dictation: OS mute warning, headset/mic disconnect
// continuing on another microphone, interrupted audio resuming itself.
//   JAY_URL=http://127.0.0.1:4000/jay/ node _tests/jay-mic.mjs   (after jay-browser.mjs fetched fixtures)
import { createRequire } from 'node:module';
const { chromium } = createRequire(import.meta.url)(process.env.PLAYWRIGHT_CORE || 'playwright-core');
const ok = (c, m) => { console.log(c ? '✓' : '✗', m); if (!c) process.exitCode = 1; };
const browser = await chromium.launch({ executablePath: process.env.CHROMIUM || undefined, args: ['--use-fake-ui-for-media-stream', '--use-fake-device-for-media-stream', '--use-file-for-fake-audio-capture=/tmp/jay-browser-check/mic48.wav', '--audio-buffer-size=2048'] });
const page = await (await browser.newContext({ viewport: { width: 1100, height: 800 }, permissions: ['microphone'] })).newPage();
const errs = []; page.on('pageerror', (e) => errs.push(e.message));
await page.addInitScript(() => localStorage.setItem('jay.debug', '1'));
await page.goto(process.env.JAY_URL || 'http://127.0.0.1:4000/jay/');
await page.waitForFunction(() => /ready/.test(document.querySelector('#loadline')?.textContent || ''), null, { timeout: 60000 });
await page.click('#rec');
await page.waitForFunction(() => document.querySelector('#dock').classList.contains('recording'), null, { timeout: 30000 });
await page.waitForTimeout(3000);
const samples = () => page.evaluate(() => document.querySelector('#clock').textContent);
// OS mutes the mic (e.g. hardware mute switch / another app) → visible warning, cleared on unmute
await page.evaluate(() => self.jayRec().stream.getAudioTracks()[0].dispatchEvent(new Event('mute')));
await page.waitForTimeout(200);
ok(await page.evaluate(() => !document.querySelector('#strip-warn').hidden && /muted by the system/.test(document.querySelector('#strip-warn').textContent)), 'system mute shows a warning');
await page.evaluate(() => self.jayRec().stream.getAudioTracks()[0].dispatchEvent(new Event('unmute')));
await page.waitForTimeout(200);
ok(await page.evaluate(() => document.querySelector('#strip-warn').hidden), 'unmute clears it');
// headset unplugged → track ends → recording continues on another microphone
const before = await page.evaluate(() => self.jayRec().stream.id);
await page.evaluate(() => self.jayRec().stream.getAudioTracks()[0].dispatchEvent(new Event('ended')));
await page.waitForTimeout(1500);
const t1 = await samples(); await page.waitForTimeout(2500); const t2 = await samples();
const after = await page.evaluate(() => ({ id: self.jayRec().stream.id, rec: document.querySelector('#dock').classList.contains('recording'), toast: [...document.querySelectorAll('.toast')].map((t) => t.textContent).join(' | ') }));
ok(after.rec && after.id !== before && t2 !== t1 && /continuing on/.test(after.toast), `mic disconnect → switched stream and kept recording (${t1} → ${t2}): ${after.toast}`);
// audio interrupted (iOS phone call / Android audio focus) → resumes itself
await page.evaluate(() => self.jayRec().ctx.suspend());
await page.waitForTimeout(1500);
ok(await page.evaluate(() => self.jayRec().ctx.state === 'running'), 'suspended audio context resumes itself');
await page.click('#rec');
await page.waitForFunction(() => !document.querySelector('.turn.live') && [...document.querySelectorAll('.turn .chip')].every((c) => !/transcrib|live|queued/.test(c.textContent)), null, { timeout: 120000 });
const dur = await page.evaluate(() => document.querySelector('.turn .dur')?.textContent);
ok(dur && dur !== '0:00', `recording across the mic switch saved (${dur})`);
ok(!errs.length, 'no page errors ' + errs.join('; '));
await browser.close();
