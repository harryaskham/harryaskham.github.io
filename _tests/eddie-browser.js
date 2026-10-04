async page => {
  const url = page.url();
  const names = ['01-house-party', '02-pub-mirrorverse', '03-riviera', '04-festival', '05-baroque-ceiling', '06-neon-city', '07-cosmic-portals', '08-carnival', '09-miniature-world', '10-stadium'];
  const errors = [];
  const failed = [];
  let stage = 'initial load';
  const onError = error => errors.push(error.message);
  const onFailed = request => failed.push(request.url());
  const check = (condition, message) => { if (!condition) throw new Error(message); };
  const state = () => page.evaluate(() => {
    const slides = [...document.querySelectorAll('.slide')];
    const current = slides.filter(slide => slide.classList.contains('is-current'));
    const photo = current[0]?.querySelector('.photo');
    return {
      count: slides.length,
      current: current.length,
      index: slides.indexOf(current[0]),
      src: photo?.currentSrc || '',
      loaded: Boolean(photo?.complete && photo.naturalWidth),
      previous: document.querySelectorAll('.slide.is-previous').length,
      paused: document.documentElement.classList.contains('paused'),
    };
  });
  const waitFor = index => page.waitForFunction(([index, name]) => {
    const slide = document.querySelectorAll('.slide')[index];
    const photo = slide?.querySelector('.photo');
    return slide?.classList.contains('is-current') && photo.complete && photo.naturalWidth && photo.currentSrc.includes(name);
  }, [index, names[index]]);
  // Freeze page timers between explicit runFor steps; image loading still uses real network time.
  const freeze = async () => page.clock.pauseAt(await page.evaluate(() => Date.now() + 50));
  // Step fake time until the target slide appears, rejecting both early and late advances.
  const advanceTo = async (index, minMs = 0) => {
    for (let elapsed = 500; elapsed <= 11_000; elapsed += 500) {
      await page.clock.runFor(500);
      await page.waitForTimeout(80);
      const now = await state();
      if (now.index === index) {
        check(elapsed >= minMs, `Slide ${index} advanced after only ${elapsed} ms`);
        await waitFor(index);
        return elapsed;
      }
    }
    throw new Error(`Slide ${index} did not advance within 11 s`);
  };
  page.on('pageerror', onError);
  page.on('requestfailed', onFailed);

  try {
    await page.clock.install();
    await page.setViewportSize({ width: 1920, height: 1080 });
    await page.goto(url);
    await waitFor(0);
    await freeze();
    check(await page.title() === 'Happy birthday, Eddie!', 'Wrong title');
    let s = await state();
    check(s.count === names.length && s.current === 1 && s.index === 0, `Initial state ${JSON.stringify(s)}`);
    check(await page.getByRole('button', { name: 'Pause slideshow' }).isVisible(), 'Pause control missing');

    stage = 'layout';
    for (const [width, height, fit, backdrop] of [[1920, 1080, 'cover', false], [1440, 900, 'cover', false], [390, 844, 'contain', true], [1024, 768, 'contain', true], [844, 390, 'contain', true], [2560, 1080, 'contain', true]]) {
      await page.setViewportSize({ width, height });
      const layout = await page.evaluate(() => {
        const slide = document.querySelector('.slide.is-current');
        const photo = slide.querySelector('.photo').getBoundingClientRect();
        return {
          overflow: document.documentElement.scrollWidth > innerWidth || document.documentElement.scrollHeight > innerHeight,
          fills: photo.left <= 0 && photo.top <= 0 && photo.right >= innerWidth && photo.bottom >= innerHeight,
          fit: getComputedStyle(slide.querySelector('.photo')).objectFit,
          backdrop: getComputedStyle(slide.querySelector('.backdrop')).display !== 'none',
        };
      });
      check(!layout.overflow && layout.fills && layout.fit === fit && layout.backdrop === backdrop, `Layout ${width}x${height}: ${JSON.stringify(layout)}`);
      await page.screenshot({ path: `eddie-${width}x${height}.png`, animations: 'disabled' });
    }

    stage = 'ten-second crossfade loop';
    await page.setViewportSize({ width: 1920, height: 1080 });
    for (let index = 1; index <= names.length; index++) {
      await advanceTo(index % names.length, index === 1 ? 0 : 6_500);
      await page.clock.runFor(2_600);
      s = await state();
      check(s.current === 1 && s.previous === 0, `Fade cleanup at ${index}: ${JSON.stringify(s)}`);
      if (index === 3) await page.screenshot({ path: 'eddie-slide-04-1920x1080.png', animations: 'disabled' });
    }

    stage = 'pause and resume';
    await page.getByRole('button', { name: 'Pause slideshow' }).click();
    await page.clock.runFor(30_000);
    s = await state();
    check(s.paused && s.index === 0, `Pause failed: ${JSON.stringify(s)}`);
    await page.getByRole('button', { name: 'Play slideshow' }).focus();
    await page.screenshot({ path: 'eddie-paused-focus-1920x1080.png', animations: 'disabled' });
    await page.keyboard.press('Space');
    await advanceTo(1, 9_500);

    stage = 'reduced motion';
    await page.emulateMedia({ reducedMotion: 'reduce' });
    const motion = await page.evaluate(() => ({
      fade: getComputedStyle(document.querySelector('.slide.is-current')).transitionDuration,
      drift: getComputedStyle(document.querySelector('.slide.is-current .photo')).animationName,
    }));
    check(motion.fade.startsWith('0.01s') && motion.drift === 'none', `Reduced motion: ${JSON.stringify(motion)}`);
    await page.emulateMedia({ reducedMotion: 'no-preference' });

    stage = 'broken next image';
    await page.route('**/images/02-pub-mirrorverse-*.webp', route => route.abort());
    await page.reload();
    await waitFor(0);
    await freeze();
    await advanceTo(2);
    await page.unroute('**/images/02-pub-mirrorverse-*.webp');

    stage = 'no-JS fallback';
    const fallbackContext = await page.context().browser().newContext({ javaScriptEnabled: false, viewport: { width: 390, height: 844 } });
    try {
      const fallback = await fallbackContext.newPage();
      await fallback.goto(url);
      await fallback.waitForFunction(() => document.querySelector('.photo').complete && document.querySelector('.photo').naturalWidth);
      check(await fallback.locator('.slide').count() === 1, 'No-JS slide count');
      check(await fallback.locator('#toggle').isHidden(), 'No-JS control visible');
      await fallback.screenshot({ path: 'eddie-no-js-390x844.png' });
    } finally { await fallbackContext.close(); }

    const unexpected = failed.filter(request => !request.includes('02-pub-mirrorverse'));
    check(errors.length === 0, `JavaScript errors: ${errors.join('; ')}`);
    check(unexpected.length === 0, `Failed requests: ${unexpected.join('; ')}`);
    return { passed: true, slides: names.length, viewports: 6, checks: ['all images', 'responsive fit', '10s loop', 'fade cleanup', 'wrap', 'pause/resume', 'keyboard', 'reduced motion', 'broken image skip', 'no-JS'] };
  } catch (error) {
    throw new Error(`${stage}: ${error.message}`);
  } finally {
    await page.unroute('**/images/02-pub-mirrorverse-*.webp').catch(() => {});
    page.off('pageerror', onError);
    page.off('requestfailed', onFailed);
  }
}
