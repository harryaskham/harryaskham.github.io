// Post-deploy smoke test for the live site: every shell file on the CDN carries the same
// build stamp, and a brand-new visitor (fresh profile) gets the built-in model ready and
// can transcribe an upload. Run after every push to main — the v6 HTTP 416 regression only
// appeared behind GitHub Pages' on-the-fly gzip, never on the local dev server.
//   node _tests/jay-smoke.mjs                      (defaults to https://a.skh.am/jay/)
//   EXPECT=jay-build:<id> node _tests/jay-smoke.mjs (also require that build to be live)
import { createRequire } from 'node:module';
import fs from 'node:fs';
const { chromium } = createRequire(import.meta.url)(process.env.PLAYWRIGHT_CORE || 'playwright-core');
const URL_ = process.env.JAY_URL || 'https://a.skh.am/jay/';
const FILES = ['index.html', 'style.css', 'app.js', 'db.js', 'audio.js', 'dsp.js', 'worker.js', 'capture-worklet.js', 'sw.js'];
const ok = (c, m) => { console.log(c ? '✓' : '✗', m); if (!c) process.exitCode = 1; };

const stamps = await Promise.all(FILES.map(async (f) => (await (await fetch(new URL(f, URL_), { cache: 'no-store' })).text()).match(/jay-build:[0-9a-z]+/)?.[0]));
const build = stamps[0];
ok(stamps.every((s) => s === build) && (!process.env.EXPECT || build === process.env.EXPECT), `CDN serves one consistent build ${build}` + (stamps.every((s) => s === build) ? '' : ` (mixed: ${[...new Set(stamps)].join(', ')})`));

// a short clinical clip (public MedASR test audio), cached in /tmp
const clip = '/tmp/jay-browser-check/1.wav';
if (!fs.existsSync(clip)) {
  fs.mkdirSync('/tmp/jay-browser-check', { recursive: true });
  fs.writeFileSync(clip, Buffer.from(await (await fetch('https://huggingface.co/csukuangfj/sherpa-onnx-medasr-ctc-en-int8-2025-12-25/resolve/main/test_wavs/1.wav')).arrayBuffer()));
}
const browser = await chromium.launch({ executablePath: process.env.CHROMIUM || undefined });
try {
  const page = await (await browser.newContext()).newPage();
  const errs = [], bad = [];
  page.on('pageerror', (e) => errs.push(e.message));
  page.on('response', (r) => { if (r.status() >= 400 && r.url().startsWith(URL_)) bad.push(`${r.status()} ${r.url().slice(URL_.length)}`); });
  const t0 = Date.now();
  await page.goto(URL_);
  await page.waitForFunction(() => /ready|Couldn/.test(document.querySelector('#loadline')?.textContent || ''), null, { timeout: 120000 });
  const line = await page.textContent('#loadline');
  ok(/ready/.test(line), `new visitor: built-in model ready in ${((Date.now() - t0) / 1000).toFixed(1)}s — ${line}`);
  ok(await page.evaluate(() => crossOriginIsolated && window.jayBuild === document.querySelector('meta[name=jay-build]')?.content), 'isolated (threads) and page/script builds agree');
  if (/ready/.test(line)) {
    await page.setInputFiles('#file', clip);
    await page.waitForFunction(() => /Moonshine|failed/.test(document.querySelector('.turn .chip')?.textContent || ''), null, { timeout: 120000 });
    const text = await page.textContent('.turn .text');
    ok(/biopsy/i.test(text) && /osteoporosis/i.test(text), 'upload transcribes: ' + text.replace(/^\d+:\d+/, '').slice(0, 70) + '…');
  }
  ok(!bad.length, 'no failed requests ' + bad.join(', '));
  ok(!errs.length, 'no page errors ' + errs.join('; '));
} finally {
  await browser.close();
}
