// Reliability checks for /jay/: crash recovery, delete-during-transcription,
// full backup → erase → restore (audio + regenerated sonograms).
// Run _tests/jay-browser.mjs first (it fetches the audio fixtures to /tmp).
//   JAY_URL=http://127.0.0.1:4000/jay/ node _tests/jay-reliability.mjs
import { createRequire } from 'node:module';
const { chromium } = createRequire(import.meta.url)(process.env.PLAYWRIGHT_CORE || 'playwright-core');
const URL_ = process.env.JAY_URL || 'http://127.0.0.1:4000/jay/';
import fs from 'fs';
let failed = 0;
const ok = (c, m) => { console.log(c ? '✓' : '✗', m); if (!c) failed++; };
const browser = await chromium.launch({ executablePath: process.env.CHROMIUM || undefined, args: [
  '--use-fake-ui-for-media-stream', '--use-fake-device-for-media-stream', '--use-file-for-fake-audio-capture=/tmp/jay-browser-check/mic48.wav%noloop', '--audio-buffer-size=2048'] });
const ctx = await browser.newContext({ viewport: { width: 1280, height: 820 }, permissions: ['microphone'], acceptDownloads: true });
const page = await ctx.newPage();
const errs = []; page.on('pageerror', (e) => errs.push(e.message));
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
ok(!errs.length, 'no page errors ' + errs.join('; '));
await browser.close();
process.exit(failed ? 1 : 0);
