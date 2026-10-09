// Update/consistency checks for /jay/ against a local build: atomic deploys, idle reload,
// no reload mid-recording, offline, half-propagated deploys refused, stale-HTML self-heal.
// Simulates deploys by rewriting the build stamp in _site/jay (restored afterwards).
//   JAY_URL=http://127.0.0.1:4000/jay/ node _tests/jay-update.mjs
import { createRequire } from 'node:module';
import fs from 'node:fs';
import { execSync } from 'node:child_process';
const { chromium } = createRequire(import.meta.url)(process.env.PLAYWRIGHT_CORE || 'playwright-core');
const URL_ = process.env.JAY_URL || 'http://127.0.0.1:4000/jay/';
const SITE = new URL('../_site/jay/', import.meta.url).pathname;
const OLD = '/tmp/jay-browser-check/v1-index.html';
fs.mkdirSync('/tmp/jay-browser-check', { recursive: true });
fs.writeFileSync(OLD, execSync('git show fdd5653a:static/jay/index.html', { cwd: new URL('..', import.meta.url).pathname }));
const STAMPED = ['index.html', 'style.css', 'app.js', 'db.js', 'audio.js', 'dsp.js', 'worker.js', 'capture-worklet.js', 'sw.js'];
const orig = Object.fromEntries(STAMPED.map((f) => [f, fs.readFileSync(SITE + f, 'utf8')]));
const A = orig['app.js'].match(/jay-build:[0-9a-z]+/)[0];
const deploy = (tok, only) => { for (const f of STAMPED) if (!only || only.includes(f)) fs.writeFileSync(SITE + f, orig[f].replaceAll(A, tok)); };
const ok = (c, m) => { console.log(c ? '✓' : '✗', m); if (!c) process.exitCode = 1; };
const browser = await chromium.launch({ executablePath: process.env.CHROMIUM || undefined, args: ['--use-fake-ui-for-media-stream', '--use-fake-device-for-media-stream'] });
try {
  const ctx = await browser.newContext({ viewport: { width: 1100, height: 800 }, permissions: ['microphone'] });
  const page = await ctx.newPage();
  const errs = []; page.on('pageerror', (e) => errs.push(e.message));
  let navs = 0; page.on('framenavigated', (f) => { if (f === page.mainFrame()) navs++; });
  await page.goto(URL_);
  await page.waitForFunction(() => /ready/.test(document.querySelector('#loadline')?.textContent || ''), null, { timeout: 60000 });
  const build = () => page.evaluate(() => [window.jayBuild, document.querySelector('meta[name=jay-build]')?.content]);
  ok((await build()).every((b) => b === A), `first visit on ${A}, controlled=${await page.evaluate(() => !!navigator.serviceWorker.controller)}`);
  // 1. a new deploy while idle → new build installs atomically and the page moves to it
  deploy('jay-build:b0b0b0b0b0');
  const n1 = navs;
  await page.evaluate(() => navigator.serviceWorker.getRegistration('./').then((r) => r.update()));
  await page.waitForFunction(() => window.jayBuild === 'jay-build:b0b0b0b0b0', null, { timeout: 30000 });
  ok((await build()).every((b) => b === 'jay-build:b0b0b0b0b0') && navs > n1, 'idle tab picks up a new deploy (page + scripts agree)');
  // 2. offline: the installed build still opens and records
  await ctx.setOffline(true);
  await page.reload(); await page.waitForTimeout(1500);
  ok((await build()).every((b) => b === 'jay-build:b0b0b0b0b0') && await page.$('#rec'), 'opens offline from the installed build');
  await ctx.setOffline(false);
  // 3. a deploy during a recording waits until the recording is stopped
  await page.click('#rec');
  await page.waitForFunction(() => document.querySelector('#dock').classList.contains('recording'), null, { timeout: 30000 });
  deploy('jay-build:c1c1c1c1c1');
  await page.evaluate(() => navigator.serviceWorker.getRegistration('./').then((r) => r.update()));
  await page.waitForTimeout(6000);
  const mid = await page.evaluate(() => ({ build: window.jayBuild, dock: document.querySelector('#dock').className, toast: [...document.querySelectorAll('.toast')].map((t) => t.textContent).join(' | ') }));
  ok(mid.build === 'jay-build:b0b0b0b0b0' && /recording/.test(mid.dock), 'no reload mid-recording ' + JSON.stringify(mid));
  await page.click('#rec');
  await page.waitForFunction(() => window.jayBuild === 'jay-build:c1c1c1c1c1', null, { timeout: 40000 });
  ok(true, 'update applied once the recording finished');
  // 4. a half-propagated deploy (mixed builds) is refused; the tab stays on a consistent build
  deploy('jay-build:d2d2d2d2d2', ['sw.js', 'index.html', 'app.js']);
  await page.evaluate(() => navigator.serviceWorker.getRegistration('./').then((r) => r.update()).catch(() => {}));
  await page.waitForTimeout(5000);
  await page.reload(); await page.waitForTimeout(1500);
  ok((await build()).every((b) => b === 'jay-build:c1c1c1c1c1'), 'mixed-build deploy refused; still consistent on the last good build');
  deploy('jay-build:d2d2d2d2d2');
  await page.evaluate(() => navigator.serviceWorker.getRegistration('./').then((r) => r.update()));
  await page.waitForFunction(() => window.jayBuild === 'jay-build:d2d2d2d2d2', null, { timeout: 40000 });
  ok(true, 'completed deploy then installs');
  // 5. recording works after all that
  await page.click('#rec'); await page.waitForTimeout(2500);
  const toast = await page.evaluate(() => document.querySelector('.toast')?.textContent || '');
  ok(await page.evaluate(() => document.querySelector('#dock').classList.contains('recording')) && !/Couldn/.test(toast), 'record starts cleanly');
  await page.click('#rec');
  ok(!errs.length, 'no page errors ' + errs.join('; '));
  // 6. the reported failure: v1-era HTML with current scripts (no service worker) heals itself
  const ctx2 = await browser.newContext({ viewport: { width: 412, height: 860 }, permissions: ['microphone'], serviceWorkers: 'block' });
  let served = 0;
  await ctx2.route('**/jay/', (r) => { served++; return r.fulfill({ status: 200, contentType: 'text/html', body: fs.readFileSync(OLD, 'utf8') }); });
  const p2 = await ctx2.newPage();
  const errs2 = []; p2.on('pageerror', (e) => errs2.push(e.message));
  await p2.goto(URL_); await p2.waitForTimeout(4000);
  ok(served === 2, `stale HTML detected → exactly one healing reload (${served} loads, no loop)`);
  await p2.click('#rec'); await p2.waitForTimeout(1500);
  const t2 = await p2.evaluate(() => document.querySelector('.toast')?.textContent || '');
  ok(!/Couldn/.test(t2) && await p2.evaluate(() => document.querySelector('#dock').classList.contains('recording')), 'even stale HTML can record now: ' + (t2 || 'no error toast'));
  ok(!errs2.length, 'no page errors on stale HTML ' + errs2.join('; '));
} finally {
  deploy(A);
  await browser.close();
}
