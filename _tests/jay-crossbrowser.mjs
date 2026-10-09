// Firefox + WebKit smoke test for /jay/: boot, isolation/threads, model load,
// upload transcription, fake-mic recording (Firefox), search, phone layout.
// Needs Playwright browsers; on Nix: PLAYWRIGHT_BROWSERS_PATH=$(nix build --print-out-paths
// nixpkgs#playwright-driver.browsers) with a matching playwright-core version.
// Run _tests/jay-browser.mjs first (it fetches the audio fixtures to /tmp).
//   JAY_URL=http://127.0.0.1:4000/jay/ node _tests/jay-crossbrowser.mjs
import { createRequire } from 'node:module';
const pw = createRequire(import.meta.url)(process.env.PLAYWRIGHT_CORE || 'playwright-core');
const URL_ = process.env.JAY_URL || 'http://127.0.0.1:4000/jay/';
const B = process.env.PLAYWRIGHT_BROWSERS_PATH || '';
const fs = await import('node:fs');
const ffExe = B && fs.readdirSync(B).find((d) => d.startsWith('firefox-'));
const engines = {
  firefox: { type: pw.firefox, exe: ffExe ? `${B}/${ffExe}/firefox/firefox` : undefined, opts: { firefoxUserPrefs: { 'media.navigator.streams.fake': true, 'media.navigator.permission.disabled': true } } },
  webkit: { type: pw.webkit, exe: undefined, opts: {} },
};
for (const name of (process.env.E || 'firefox,webkit').split(',')) {
  const e = engines[name];
  const errs = [], logs = [];
  let browser;
  try {
    browser = await e.type.launch({ executablePath: e.exe, ...e.opts });
    const page = await (await browser.newContext({ viewport: { width: 1280, height: 800 } })).newPage();
    page.on('pageerror', (x) => errs.push(x.message));
    page.on('console', (m) => { if (m.type() === 'error' || m.type() === 'warning') logs.push(m.text().slice(0, 200)); });
    const t0 = Date.now();
    await page.goto(URL_);
    let ready = true;
    try { await page.waitForFunction(() => /ready|Couldn/.test(document.querySelector('#loadline')?.textContent || ''), null, { timeout: 90000 }); } catch { ready = false; }
    const st = await page.evaluate(() => ({ iso: self.crossOriginIsolated, sw: !!navigator.serviceWorker?.controller, line: document.querySelector('#loadline')?.textContent, mr: typeof MediaRecorder, ds: typeof DecompressionStream }));
    console.log(`[${name}] ${browser.version()} boot ${ready ? 'ok' : 'STUCK'} ${((Date.now() - t0) / 1000).toFixed(1)}s`, JSON.stringify(st));
    if (ready && /ready/.test(st.line)) {
      await page.setInputFiles('#file', '/tmp/jay-browser-check/upload.wav');
      const t1 = Date.now();
      try {
        await page.waitForFunction(() => { const c = document.querySelector('.turn .chip'); return c && /Moonshine|failed/.test(c.textContent); }, null, { timeout: 120000 });
        console.log(`[${name}] upload ${((Date.now() - t1) / 1000).toFixed(1)}s:`, (await page.textContent('.turn .text')).slice(0, 110), '| sono', await page.evaluate(() => document.querySelector('.turn .scrub canvas')?.width));
      } catch { console.log(`[${name}] upload STUCK`, await page.evaluate(() => document.querySelector('.turn')?.innerText.slice(0, 200))); }
      if (name === 'firefox') {
        await page.keyboard.press('r'); await page.waitForTimeout(6000);
        const live = await page.evaluate(() => ({ rec: document.querySelector('#dock').classList.contains('recording'), clock: document.querySelector('#clock').textContent }));
        await page.keyboard.press('r');
        await page.waitForFunction(() => !document.querySelector('.turn.live'), null, { timeout: 30000 }).catch(() => {});
        await page.waitForTimeout(3000);
        console.log(`[${name}] record`, JSON.stringify(live), 'turns', await page.evaluate(() => [...document.querySelectorAll('.turn .chip')].map((c) => c.textContent).join(' | ')));
      }
      await page.fill('#search', 'biopsy'); await page.waitForTimeout(300);
      console.log(`[${name}] search hits`, await page.evaluate(() => document.querySelectorAll('.turn mark').length));
      await page.fill('#search', '');
      await page.setViewportSize({ width: 390, height: 844 }); await page.waitForTimeout(300);
      console.log(`[${name}] phone overflow`, await page.evaluate(() => document.documentElement.scrollWidth));
      await page.screenshot({ path: `/tmp/jay-browser-check/xb-${name}.png` });
    }
    console.log(`[${name}] pageerrors:`, errs.length ? errs : 'none');
    if (logs.length) console.log(`[${name}] console:`, [...new Set(logs)].slice(0, 8));
  } catch (x) { console.log(`[${name}] LAUNCH/RUN FAIL`, x.message.slice(0, 300)); }
  await browser?.close();
}
