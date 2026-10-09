// Model downloads on a flaky network, against a local MedASR mirror that drops the first
// connection part-way and can throttle: Range resume + checksum, switching away cancels,
// Cancel in Models, and "Get" downloading in the background without evicting the loaded model.
// Needs MedASR files (model_int8.onnx, tokens.txt) in $MEDASR_DIR.
//   MEDASR_DIR=/path JAY_URL=http://127.0.0.1:4000/jay/ node _tests/jay-downloads.mjs
import { createRequire } from 'node:module';
import http from 'node:http';
import fs from 'node:fs';
import zlib from 'node:zlib';
const gzCache = {};
const { chromium } = createRequire(import.meta.url)(process.env.PLAYWRIGHT_CORE || 'playwright-core');
const URL_ = process.env.JAY_URL || 'http://127.0.0.1:4000/jay/';
const DIR = process.env.MEDASR_DIR;
if (!DIR || !fs.existsSync(`${DIR}/model_int8.onnx`)) { console.log('skip: set MEDASR_DIR to a folder with model_int8.onnx and tokens.txt'); process.exit(0); }
// Serves MedASR files with CORS + Range; drops the first connection for each file part-way,
// and can throttle, to exercise Jay's resumable/cancellable downloads.
const port = +(process.env.FLAKY_PORT || 4099);
const dropped = new Set(); const log = [];
const server = http.createServer((req, res) => {
  const h = { 'Access-Control-Allow-Origin': '*', 'Access-Control-Allow-Headers': 'Range', 'Access-Control-Expose-Headers': 'Content-Length, Content-Range', 'Cross-Origin-Resource-Policy': 'cross-origin' };
  if (req.method === 'OPTIONS') { res.writeHead(204, h); return res.end(); }
  const url = new URL(req.url, 'http://x'); const name = url.pathname.split('/').pop();
  if (url.pathname === '/log') { res.writeHead(200, { ...h, 'content-type': 'application/json' }); return res.end(JSON.stringify(log)); }
  const file = `${DIR}/${name}`;
  if (!fs.existsSync(file)) { res.writeHead(404, h); return res.end(); }
  const size = fs.statSync(file).size;
  const m = /bytes=(\d+)-/.exec(req.headers.range || ''); const start = m ? +m[1] : 0;
  const slow = url.pathname.startsWith('/slow/');
  if (url.pathname.startsWith('/gz/')) {
    // Like GitHub Pages: compress on the fly; Content-Length is the compressed size; a Range past
    // the compressed length is 416.
    const gz = gzCache[name] ||= zlib.gzipSync(fs.readFileSync(file), { level: 1 });
    log.push({ name, gz: true, range: req.headers.range || null });
    const st = m ? +m[1] : 0;
    if (st >= gz.length) { res.writeHead(416, h); return res.end(); }
    res.writeHead(m ? 206 : 200, { ...h, 'Content-Encoding': 'gzip', 'Content-Length': gz.length - st, ...(m ? { 'Content-Range': `bytes ${st}-${gz.length - 1}/${gz.length}` } : {}) });
    return res.end(gz.subarray(st));
  }
  log.push({ name, slow, range: req.headers.range || null, at: Date.now() });
  res.writeHead(m ? 206 : 200, { ...h, 'Content-Length': size - start, ...(m ? { 'Content-Range': `bytes ${start}-${size - 1}/${size}` } : {}), 'Content-Type': 'application/octet-stream' });
  const s = fs.createReadStream(file, { start, highWaterMark: 256 * 1024 });
  let sent = 0;
  const dropAt = !dropped.has(name) && size > 1e6 && !slow ? Math.floor(size * 0.4) : Infinity;
  s.on('data', (c) => {
    sent += c.length;
    if (sent > dropAt) { dropped.add(name); log.push({ name, slow, dropAfter: start + sent }); s.destroy(); res.socket.destroy(); return; }
    const ok = res.write(c);
    if (slow) { s.pause(); setTimeout(() => s.resume(), 120); }
    else if (!ok) { s.pause(); res.once('drain', () => s.resume()); }
  });
  s.on('end', () => res.end());
  req.on('close', () => { if (!res.writableEnded && !(dropped.has(name) && sent > dropAt)) { log.push({ name, slow, clientClosedAfter: start + sent }); s.destroy(); } });
}).listen(port);


const ok = (c, m) => { console.log(c ? '✓' : '✗', m); if (!c) process.exitCode = 1; };
const S = `http://127.0.0.1:${port}`;
const custom = (id, base) => ({ id, name: id === 'flaky' ? 'MedASR (flaky mirror)' : 'MedASR (slow mirror)', kind: 'ctc', format: 'medasr', custom: true, size: 108e6, tags: [], blurb: base,
  files: { model: { url: `${base}/model_int8.onnx`, sha256: '6672e7bf25ff6c7fa2f6b620dcae6127431614390665f08dd3ffbd9e72e23309', size: 108083856 }, tokens: { url: `${base}/tokens.txt` } } });
const browser = await chromium.launch({ executablePath: process.env.CHROMIUM || undefined });
const page = await (await browser.newContext({ viewport: { width: 1100, height: 800 } })).newPage();
const errs = []; page.on('pageerror', (e) => errs.push(e.message));
const stages = []; page.on('console', (m) => { const t = m.text(); const s = /\[jay\] model\s.*/.test(t); if (s) stages.push(t); });
await page.addInitScript(([a, b, g]) => { localStorage.setItem('jay.debug', '1'); if (!localStorage.getItem('jay.prefs')) localStorage.setItem('jay.prefs', JSON.stringify({ custom: [a, b, g] })); }, [custom('flaky', S), custom('slow', S + '/slow'), { ...custom('gz', S + '/gz'), name: 'MedASR (gzip CDN)' }]);
await page.goto(URL_);
await page.waitForFunction(() => /ready/.test(document.querySelector('#loadline')?.textContent || ''), null, { timeout: 60000 });
// 0. a CDN that gzips on the fly (GitHub Pages): Content-Length ≠ decoded size must not break downloads
await page.click('#model-chip'); await page.click('#menu button:has-text("gzip CDN")');
await page.waitForFunction(() => /ready|error/.test(document.querySelector('#model-chip .dot')?.className || ''), null, { timeout: 180000 });
ok(await page.evaluate(() => document.querySelector('#model-chip .dot').classList.contains('ready')), 'model from a gzip-on-the-fly CDN loads: ' + (await page.evaluate(() => document.querySelector('#model-chip').title)));
await page.click('#model-chip'); await page.click('#menu button:has-text("Moonshine Tiny")');
await page.waitForFunction(() => document.querySelector('#model-chip .dot')?.classList.contains('ready') && /Tiny/.test(document.querySelector('#model-name').textContent), null, { timeout: 60000 });
// 1. resume after a dropped connection
const seenRetry = page.waitForFunction(() => /Connection dropped — resuming/.test(document.querySelector('#loadline')?.textContent || ''), null, { timeout: 60000 }).then(() => true).catch(() => false);
await page.click('#model-chip'); await page.click('#menu button:has-text("flaky mirror")');
const retryShown = await seenRetry;
await page.waitForFunction(() => document.querySelector('#model-chip .dot')?.classList.contains('ready'), null, { timeout: 180000 });
const log1 = await (await fetch(S + '/log')).json();
const model1 = log1.filter((l) => l.name === 'model_int8.onnx' && !l.slow);
ok(retryShown && model1.some((l) => l.dropAfter) && model1.some((l) => /bytes=\d+-/.test(l.range || '')), `dropped at ${model1.find((l) => l.dropAfter)?.dropAfter} bytes, resumed with ${model1.find((l) => l.range)?.range}; UI said "Connection dropped — resuming"`);
await page.setInputFiles('#file', '/tmp/jay-browser-check/0.wav');
await page.waitForFunction(() => /MedASR \(flaky/.test(document.querySelector('.turn .chip')?.textContent || ''), null, { timeout: 120000 });
ok(/IMPRESSION/i.test(await page.textContent('.turn .text')), 'resumed model is intact (checksum passed) and transcribes the report');
// 2. switching away mid-download cancels it instead of waiting
await page.click('#model-chip'); await page.click('#menu button:has-text("slow mirror")');
await page.waitForFunction(() => /Fetching MedASR \(slow/.test(document.querySelector('#model-chip')?.title || ''), null, { timeout: 30000 });
await page.waitForTimeout(1500);
const t0 = Date.now();
await page.click('#model-chip'); await page.click('#menu button:has-text("Moonshine Tiny")');
await page.waitForFunction(() => document.querySelector('#model-chip .dot')?.classList.contains('ready') && /Tiny/.test(document.querySelector('#model-name').textContent), null, { timeout: 30000 });
const switched = (Date.now() - t0) / 1000;
await page.waitForTimeout(1000);
const log2 = await (await fetch(S + '/log')).json();
const closed = log2.find((l) => l.slow && l.clientClosedAfter && l.name === 'model_int8.onnx');
ok(switched < 10 && closed, `switching to Tiny took ${switched.toFixed(1)}s and cancelled the slow download at ${closed?.clientClosedAfter} bytes`);
await page.click('#settings-btn'); await page.click('[data-tab=models]'); await page.waitForTimeout(300);
ok(await page.evaluate(() => !/Downloading/.test(document.querySelector('[data-model=slow]')?.textContent || '')), 'the switched-away model no longer shows as downloading');
await page.keyboard.press('Escape');
// 3. explicit cancel from the Models sheet
await page.click('#settings-btn'); await page.click('[data-tab=models]');
await page.click('[data-model=slow] [data-act=get-model]');
await page.waitForSelector('[data-model=slow] [data-act=cancel-model]', { timeout: 30000 });
await page.click('[data-model=slow] [data-act=cancel-model]');
await page.waitForTimeout(1200);
ok(await page.evaluate(() => !!document.querySelector('[data-model=slow] [data-act=get-model]') && /Tiny/.test(document.querySelector('#model-name').textContent)), 'Cancel in the Models sheet stops the download and offers Get again');
// 4. "Get" downloads in the background without evicting the model in use
const log3a = (await (await fetch(S + '/log')).json()).length;
// fast mirror for the background download (the slow one would take minutes)
await page.evaluate((u) => { const p = JSON.parse(localStorage.getItem('jay.prefs')); p.custom.find((m) => m.id === 'slow').files.model.url = u; localStorage.setItem('jay.prefs', JSON.stringify(p)); }, `${S}/model_int8.onnx`);
await page.reload(); await page.waitForFunction(() => /ready/.test(document.querySelector('#loadline')?.textContent || '') || document.querySelector('#model-chip .dot.ready'), null, { timeout: 60000 });
await page.click('#settings-btn'); await page.click('[data-tab=models]');
await page.click('[data-model=slow] [data-act=get-model]');
await page.waitForFunction(() => /downloaded/.test([...document.querySelectorAll('.toast')].map((t) => t.textContent).join(' ')), null, { timeout: 180000 });
ok(await page.evaluate(() => /Tiny/.test(document.querySelector('#model-name').textContent) && document.querySelector('#model-chip .dot').classList.contains('ready')), 'Get downloads MedASR in the background; Moonshine Tiny stays loaded and selected');
ok(!errs.length, 'no page errors ' + errs.join('; '));
await browser.close();
server.close();
