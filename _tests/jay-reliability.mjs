// Reliability checks for /jay/: crash recovery, delete-during-transcription,
// full backup → erase → restore (audio + regenerated sonograms).
// Run _tests/jay-browser.mjs first (it fetches the audio fixtures to /tmp).
//   JAY_URL=http://127.0.0.1:4000/jay/ node _tests/jay-reliability.mjs
import { createRequire } from 'node:module';
import fs from 'node:fs';
const { chromium } = createRequire(import.meta.url)(process.env.PLAYWRIGHT_CORE || 'playwright-core');
const URL_ = process.env.JAY_URL || 'http://127.0.0.1:4000/jay/';
let failed = 0;
const ok = (c, m) => { console.log(c ? '✓' : '✗', m); if (!c) failed++; };
const browser = await chromium.launch({ executablePath: process.env.CHROMIUM || undefined, args: [
  '--use-fake-ui-for-media-stream', '--use-fake-device-for-media-stream', '--use-file-for-fake-audio-capture=/tmp/jay-browser-check/mic48.wav%noloop', '--audio-buffer-size=2048'] });
const ctx = await browser.newContext({ viewport: { width: 1280, height: 820 }, permissions: ['microphone'], acceptDownloads: true });
const page = await ctx.newPage();
const errs = []; page.on('pageerror', (e) => errs.push(e.message));
const jaylog = []; page.on('console', (m) => { if (m.text().startsWith('[jay]')) jaylog.push(m.text().slice(0, 120)); });
await page.addInitScript(() => localStorage.setItem('jay.debug', '1'));
await page.goto(URL_);
await page.waitForFunction(() => /ready/.test(document.querySelector('#loadline')?.textContent || ''), null, { timeout: 120000 });
// 1. crash mid-recording → recover on reload
await page.keyboard.press('r');
await page.waitForTimeout(12000);
await page.reload();
await page.waitForFunction(() => document.querySelector('.turn .chip') && !/queued|transcrib|live/.test(document.querySelector('.turn .chip').textContent), null, { timeout: 120000 });
const rec = await page.evaluate(() => ({ text: document.querySelector('.turn .text').textContent, dur: document.querySelector('.turn .dur')?.textContent, toast: document.querySelector('.toast')?.textContent }));
ok(/degrees|celsius|heart/i.test(rec.text) && rec.dur !== '0:00', `interrupted dictation recovered (${rec.dur}): ${rec.text.slice(0, 60)}`);
// 2. delete during transcription stays deleted
await page.setInputFiles('#file', '/tmp/jay-browser-check/upload.wav');
await page.waitForFunction(() => /transcrib/.test([...document.querySelectorAll('.turn .chip')].at(-1)?.textContent || ''), null, { timeout: 30000 });
const before = await page.evaluate(() => document.querySelectorAll('.turn').length);
const last = page.locator('.turn').last();
await last.hover(); await last.locator('[data-act=turn-more]').click();
await page.click('#menu button:has-text("Delete clip")');
await page.waitForTimeout(5000);
await page.reload(); await page.waitForTimeout(2500);
ok((await page.evaluate(() => document.querySelectorAll('.turn').length)) === before - 1, 'deleted-while-transcribing clip stays deleted after reload');
// 3. backup (text only) → erase → restore; sonogram regenerated from audio? (text-only has no audio) → use full backup
await page.click('#store-meter'); await page.waitForTimeout(400);
const [dl] = await Promise.all([page.waitForEvent('download'), page.click('[data-act=backup-audio]')]);
const file = '/tmp/jay-browser-check/backup.json'; await dl.saveAs(file);
const bk = JSON.parse(fs.readFileSync(file, 'utf8'));
ok(bk.jay === 1 && bk.sessions.length && Object.keys(bk.audio).length, `backup has ${bk.sessions.length} sessions, ${bk.turns.length} turns, ${Object.keys(bk.audio).length} audio, ${(fs.statSync(file).size / 1e6).toFixed(2)} MB`);
await page.click('[data-act=clear-all]'); await page.click('[data-act=clear-all]');
await page.waitForTimeout(600);
ok((await page.evaluate(() => document.querySelectorAll('.turn').length)) === 0, 'erase everything');
await page.setInputFiles('[data-act=restore-file]', file);
await page.waitForTimeout(1500);
await page.keyboard.press('Escape');
await page.click('#sessions .sess');
await page.waitForFunction(() => [...document.querySelectorAll('.turn .scrub canvas')].every((c) => c.width > 0), null, { timeout: 30000 }).catch(() => {});
await page.waitForTimeout(3000);
const restored = await page.evaluate(() => ({ n: document.querySelectorAll('.turn').length, sono: [...document.querySelectorAll('.turn')].map((t) => !!t.querySelector('.scrub canvas')?.width) }));
ok(restored.n >= 1 && restored.sono.every(Boolean), `restored ${restored.n} turns, sonograms regenerated ${JSON.stringify(restored.sono)}`);
await page.click('.turn .play'); await page.waitForTimeout(800);
ok(await page.evaluate(() => !document.querySelector('#player').paused), 'restored audio plays');
// 4. corrections: learn from an edit, then apply to a new upload; VTT export; keyboard help
await page.evaluate(() => document.querySelector('#player').pause());
const nBefore = await page.evaluate(() => document.querySelectorAll('.turn').length);
await page.setInputFiles('#file', '/tmp/jay-browser-check/upload.wav');
try {
  await page.waitForFunction((n) => document.querySelectorAll('.turn').length > n && [...document.querySelectorAll('.turn .chip')].every((c) => !/transcrib|queued/.test(c.textContent)), nBefore, { timeout: 40000 });
} catch {
  await page.evaluate(() => self.jayWorker?.()?.postMessage({ type: 'debug' }));
  await page.waitForTimeout(500);
  console.log('STALL', JSON.stringify(await page.evaluate(() => [...document.querySelectorAll('.turn')].map((t) => t.querySelector('.chip')?.textContent))));
  console.log(jaylog.filter((l) => !/segments mv|partial/.test(l)).slice(-16).join('\n'));
  process.exit(2);
}
const tgt = page.locator('.turn').last();
await tgt.hover(); await tgt.locator('[data-act=edit]').click();
const segTexts = await tgt.locator('.seg').evaluateAll((els) => els.map((e) => e.textContent));
const segIdx = segTexts.findIndex((t) => /Heart rate/.test(t));
await tgt.locator('.seg').nth(segIdx).evaluate((el) => { el.focus(); el.textContent = el.textContent.replace('Heart rate', 'Pulse'); });
await tgt.locator('[data-act=edit-done]').click();
await page.waitForTimeout(300);
const tst = await page.evaluate(() => document.querySelector('.toast')?.textContent || '');
ok(/Always write “Heart rate” as “Pulse”/.test(tst), 'edit suggests a correction: ' + tst.slice(0, 60));
await page.click('.toast button:has-text("Remember")');
await page.setInputFiles('#file', '/tmp/jay-browser-check/upload.wav');
await page.waitForFunction(() => [...document.querySelectorAll('.turn .chip')].every((c) => !/transcrib|queued/.test(c.textContent)), null, { timeout: 120000 });
const newest = await page.locator('.turn').last().locator('.text').textContent();
ok(/Pulse is 72/.test(newest) && !/Heart rate/.test(newest), 'correction applied to the next transcript');
const [vtt] = await Promise.all([page.waitForEvent('download'), (async () => { await page.click('[data-act=export-session]'); await page.click('#menu button:has-text("Subtitles")'); })()]);
const vttText = fs.readFileSync(await vtt.path(), 'utf8');
ok(/^WEBVTT/.test(vttText) && /\d\d:\d\d:\d\d\.\d{3} --> /.test(vttText), 'WebVTT export');
await page.keyboard.press('?');
ok(await page.evaluate(() => document.querySelector('#keys').open), 'keyboard help opens with ?');
await page.keyboard.press('Escape');
// 5. a hung engine: the watchdog restarts it, the clip fails cleanly, "Try again" recovers
await page.evaluate(() => localStorage.setItem('jay.stall', '6000'));
await page.reload(); await page.waitForFunction(() => /ready/.test(document.querySelector('#model-chip')?.title || '') || document.querySelector('#model-chip .dot.ready'), null, { timeout: 60000 });
await page.evaluate(() => self.jayWorker().postMessage({ type: 'debug-hang' }));
const n0 = await page.evaluate(() => document.querySelectorAll('.turn').length);
await page.setInputFiles('#file', '/tmp/jay-browser-check/upload.wav');
await page.waitForFunction((n) => document.querySelectorAll('.turn').length > n && /failed/.test([...document.querySelectorAll('.turn .chip')].at(-1).textContent), n0, { timeout: 60000 });
ok(true, 'hung engine detected; clip failed cleanly: ' + (await page.locator('.turn').last().locator('.foot').textContent()));
await page.locator('.turn').last().locator('[data-act=retry]').click();
await page.waitForFunction(() => /Moonshine/.test([...document.querySelectorAll('.turn .chip')].at(-1).textContent), null, { timeout: 60000 });
ok(/temperature/i.test(await page.locator('.turn').last().locator('.text').textContent()), 'Try again transcribes on the restarted engine');
await page.evaluate(() => localStorage.removeItem('jay.stall'));
ok(!errs.length, 'no page errors ' + errs.join('; '));
await browser.close();
process.exit(failed ? 1 : 0);
