async page => {
  const url = page.url();
  const errors = [];
  let stage = 'metadata';
  const onError = error => errors.push(error.message);
  const check = (condition, message) => { if (!condition) throw new Error(message); };
  page.on('pageerror', onError);
  try {
    await page.goto(url);
    await page.waitForFunction(() => Number.isFinite(document.querySelector('audio').duration));
    check(await page.title() === 'All-Star Pars', 'Wrong title');
    check(await page.locator('audio').evaluate(a => a.loop && a.paused && !a.autoplay && !a.muted), 'Initial audio state');
    check(await page.locator('.artwork').evaluate(i => i.complete && i.naturalWidth === 1024 && i.naturalHeight === 1024), 'Artwork missing');

    for (const [width, height] of [[1440, 960], [390, 844], [320, 568], [1280, 600], [844, 390]]) {
      await page.setViewportSize({ width, height });
      const layout = await page.evaluate(() => {
        const image = document.querySelector('.artwork').getBoundingClientRect();
        const readout = document.querySelector('.readout').getBoundingClientRect();
        const cells = [...document.querySelector('.readout').children].map(e => e.getBoundingClientRect());
        return {
          overflow: document.documentElement.scrollWidth > innerWidth,
          vertical: document.documentElement.scrollHeight > innerHeight,
          square: Math.abs(image.width - image.height) < 1,
          readoutFits: cells.every((r, i) => r.left >= readout.left - 1 && r.right <= readout.right + 1 && (!i || r.left >= cells[i - 1].right)),
        };
      });
      check(!layout.overflow && !layout.vertical && layout.square && layout.readoutFits, `Layout ${width}x${height}: ${JSON.stringify(layout)}`);
      await page.screenshot({ path: `par-${width}x${height}.png` });
    }

    stage = 'play and pause';
    // Exercise the real MP3 decoder and native loop without making the test audible.
    await page.locator('audio').evaluate(a => { a.muted = true; });
    await page.getByRole('button', { name: 'Play', exact: true }).click();
    await page.waitForFunction(() => document.querySelector('audio').currentTime > .25);
    await page.getByRole('button', { name: 'Pause', exact: true }).click();
    const stopped = await page.locator('audio').evaluate(a => a.currentTime);
    await page.waitForTimeout(300);
    check(await page.locator('audio').evaluate(a => a.paused && a.currentTime) === stopped, 'Pause did not stop playback');

    stage = 'seeking';
    const slider = page.getByRole('slider', { name: 'Playback position' });
    await slider.evaluate(s => { s.value = '30'; s.dispatchEvent(new Event('input', { bubbles: true })); });
    await page.waitForFunction(() => Math.abs(document.querySelector('audio').currentTime - 30) < .2);
    await slider.focus();
    await page.keyboard.press('ArrowRight');
    check(await page.locator('audio').evaluate(a => a.currentTime > 30), 'Keyboard seeking failed');
    await page.getByRole('button', { name: 'Play', exact: true }).click();
    await page.waitForFunction(() => document.querySelector('audio').currentTime > 30.25);
    stage = 'native loop';
    await page.locator('audio').evaluate(a => { a.currentTime = a.duration - .25; });
    await page.waitForFunction(() => {
      const a = document.querySelector('audio');
      return !a.paused && a.currentTime < 1.5;
    }, null, { timeout: 10000 });
    await page.getByRole('button', { name: 'Pause', exact: true }).click();

    stage = 'rapid toggle and keyboard';
    // Rapid start/cancel must not leave a stale error or a stuck Pause button.
    for (let i = 0; i < 3; i++) {
      await page.locator('#toggle').click();
      await page.locator('#toggle').click();
    }
    await page.waitForTimeout(200);
    check(await page.locator('audio').evaluate(a => a.paused), 'Rapid toggle left audio playing');
    check(await page.locator('#status').textContent() === '', 'Stale playback error');
    await page.setViewportSize({ width: 390, height: 844 });
    await page.locator('#toggle').focus();
    await page.keyboard.press('Space');
    await page.waitForFunction(() => !document.querySelector('audio').paused);
    await page.keyboard.press('Space');
    await page.emulateMedia({ reducedMotion: 'reduce' });
    check(await page.locator('#toggle').evaluate(b => getComputedStyle(b).transitionDuration === '0s'), 'Reduced motion ignored');
    await page.screenshot({ path: 'par-keyboard-390x844.png' });

    stage = 'network error and retry';
    await page.route('**/par.mp3', route => route.abort());
    await page.reload();
    await page.locator('#download').waitFor({ state: 'visible' });
    check(await page.getByRole('button', { name: 'Play', exact: true }).isVisible(), 'Error is not retryable');
    await page.screenshot({ path: 'par-error-390x844.png' });
    await page.unroute('**/par.mp3');
    await page.locator('audio').evaluate(a => { a.muted = true; });
    await page.getByRole('button', { name: 'Play', exact: true }).click();
    await page.waitForFunction(() => document.querySelector('audio').currentTime > .25);
    check(await page.locator('#download').isHidden(), 'Error did not clear after retry');
    await page.getByRole('button', { name: 'Pause', exact: true }).click();

    stage = 'no-JS fallback';
    const fallbackContext = await page.context().browser().newContext({ javaScriptEnabled: false, viewport: { width: 390, height: 844 } });
    try {
      const fallback = await fallbackContext.newPage();
      await fallback.goto(url);
      check(await fallback.locator('audio').isVisible(), 'No-JS native player missing');
      check(await fallback.locator('#player').isHidden(), 'No-JS custom controls exposed');
      await fallback.screenshot({ path: 'par-no-js-390x844.png' });
    } finally { await fallbackContext.close(); }
    check(errors.length === 0, `JavaScript errors: ${errors.join('; ')}`);
    return { passed: true, viewports: 5, checks: ['media decode', 'play/pause/resume', 'seek + keyboard', 'native loop', 'rapid toggle', 'error + retry', 'reduced motion', 'no-JS fallback', 'no overflow'] };
  } catch (error) {
    throw new Error(`${stage}: ${error.message}`);
  } finally {
    await page.unroute('**/par.mp3');
    await page.locator('audio').evaluate(a => a.pause()).catch(() => {});
    page.off('pageerror', onError);
  }
}
