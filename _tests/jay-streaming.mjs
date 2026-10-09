// Streaming check with clean audio: a debug fake microphone plays a clip into the real live
// pipeline at realtime pace. Committed text must only grow and the live partial stay short.
//   MODEL=medasr CLIP=/tmp/jay-browser-check/0.wav JAY_URL=http://127.0.0.1:4000/jay/ node _tests/jay-streaming.mjs
import { createRequire } from 'node:module';
import fs from 'node:fs';
const { chromium } = createRequire(import.meta.url)(process.env.PLAYWRIGHT_CORE || 'playwright-core');
const model = process.env.MODEL || 'medasr', clip = process.env.CLIP || '/tmp/jay-browser-check/0.wav';
const b = fs.readFileSync(clip); const pcm16 = new Int16Array(b.buffer.slice(b.byteOffset + 44));
const browser = await chromium.launch({ executablePath: process.env.CHROMIUM || undefined });
const page = await (await browser.newContext({ viewport: { width: 1100, height: 900 } })).newPage();
const errs = []; page.on('pageerror', (e) => errs.push(e.message));
await page.addInitScript((m) => { localStorage.setItem('jay.prefs', JSON.stringify({ model: m })); localStorage.setItem('jay.debug', '1'); }, model);
await page.goto(process.env.JAY_URL || 'http://127.0.0.1:4000/jay/');
await page.waitForFunction(() => document.querySelector('#model-chip .dot')?.classList.contains('ready'), null, { timeout: 300000 });
await page.evaluate((b64) => { const u = Uint8Array.from(atob(b64), (c) => c.charCodeAt(0)); const i = new Int16Array(u.buffer); const f = new Float32Array(16000 + i.length); for (let k = 0; k < i.length; k++) f[16000 + k] = i[k] / 32768; window.jayFakeMic = f; }, Buffer.from(pcm16.buffer).toString('base64'));
await page.keyboard.press('r');
const dur = pcm16.length / 16000 + 1, t0 = Date.now(); let maxPW = 0, flips = 0, lastCommitted = '';
while (Date.now() - t0 < (dur + 3) * 1000) {
  await page.waitForTimeout(500);
  const st = await page.evaluate(() => { const t = document.querySelector('.turn.live'); if (!t) return null; const p = [...t.querySelectorAll('.partial')].map((e) => e.textContent).join(' '); return { c: [...t.querySelectorAll('.seg')].map((e) => e.textContent).join(' '), pw: p.split(/\s+/).filter(Boolean).length }; });
  if (!st) continue;
  maxPW = Math.max(maxPW, st.pw);
  if (lastCommitted && !st.c.startsWith(lastCommitted)) flips++; // committed text must only ever grow
  lastCommitted = st.c;
}
await page.keyboard.press('r');
await page.waitForFunction(() => !document.querySelector('.turn.live') && [...document.querySelectorAll('.turn .chip')].every((c) => !/transcrib|live|queued/.test(c.textContent)), null, { timeout: 120000 });
console.log(`${model}: max live partial ${maxPW} words · committed text rewritten ${flips}×`);
if (flips || maxPW > 40 || errs.length) process.exitCode = 1;
console.log(await page.evaluate(() => [...document.querySelectorAll('.turn .text p')].map((p) => p.innerText.replace(/\n/g, ' ')).join('\n')));
console.log('errors', errs);
await browser.close();
