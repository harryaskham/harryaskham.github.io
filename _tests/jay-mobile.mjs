// Phone checks for /jay/: Android share-target flow (OS share sheet → service worker →
// new session transcribing the shared file) and the back gesture closing the sessions drawer.
//   JAY_URL=http://127.0.0.1:4000/jay/ node _tests/jay-mobile.mjs   (after jay-browser.mjs fetched fixtures)
import { createRequire } from 'node:module';
import fs from 'node:fs';
const { chromium } = createRequire(import.meta.url)(process.env.PLAYWRIGHT_CORE || 'playwright-core');
const URL_ = process.env.JAY_URL || 'http://127.0.0.1:4000/jay/';
const ok = (c, m) => { console.log(c ? '✓' : '✗', m); if (!c) process.exitCode = 1; };
const browser = await chromium.launch({ executablePath: process.env.CHROMIUM || undefined });
const ctx = await browser.newContext({ viewport: { width: 390, height: 844 }, deviceScaleFactor: 2, isMobile: true, hasTouch: true });
const page = await ctx.newPage();
const errs = []; page.on('pageerror', (e) => errs.push(e.message));
await page.goto(URL_);
await page.waitForFunction(() => /ready/.test(document.querySelector('#loadline')?.textContent || ''), null, { timeout: 60000 });
const mf = await page.evaluate(() => fetch('manifest.webmanifest').then((r) => r.json()));
ok(mf.share_target?.action === './share' && mf.share_target.params.files[0].name === 'audio', 'manifest declares an audio share target');
// simulate the OS share sheet: multipart POST to ./share, as Android Chrome does for an installed PWA
const b64 = fs.readFileSync('/tmp/jay-browser-check/upload.wav').toString('base64');
const red = await page.evaluate(async (b64) => {
  const bytes = Uint8Array.from(atob(b64), (c) => c.charCodeAt(0));
  const fd = new FormData();
  fd.append('audio', new File([bytes], 'Voice note 2026-10-09.opus.wav', { type: 'application/octet-stream' }));
  const r = await fetch('./share', { method: 'POST', body: fd, redirect: 'manual' });
  return { type: r.type, status: r.status };
}, b64);
ok(red.type === 'opaqueredirect' || red.status === 303, `service worker accepts the share and redirects (${JSON.stringify(red)})`);
await page.goto(URL_ + '?shared=1');
await page.waitForFunction(() => /Moonshine/.test(document.querySelector('.turn .chip')?.textContent || ''), null, { timeout: 90000 });
const r = await page.evaluate(async () => ({ name: document.querySelector('.turn .kind span')?.textContent, text: document.querySelector('.turn .text')?.textContent.slice(0, 60), url: location.href, left: (await (await caches.open('jay-share')).keys()).length }));
ok(/Voice note/.test(r.name) && /temperature/i.test(r.text) && !r.url.includes('shared') && r.left === 0, `shared file transcribed in a new session: ${JSON.stringify(r)}`);
// back gesture closes the drawer instead of leaving the session
const before = page.url();
await page.click('#menu-btn'); await page.waitForTimeout(350);
ok(await page.evaluate(() => document.querySelector('#app').classList.contains('drawer')), 'drawer opens');
await page.goBack(); await page.waitForTimeout(350);
ok(!(await page.evaluate(() => document.querySelector('#app').classList.contains('drawer'))) && page.url() === before && await page.$('.turn'), 'back closes the drawer and stays in the session');
await page.click('#menu-btn'); await page.waitForTimeout(300); await page.click('#scrim', { position: { x: 380, y: 400 } }); await page.waitForTimeout(300);
ok(page.url() === before, 'tapping outside closes it without changing the page');
ok(!errs.length, 'no page errors ' + errs.join('; '));
await browser.close();
