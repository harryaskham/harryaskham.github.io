const SLIDES = [
  ['01-house-party', 'A packed living-room party full of Eddies under giant balloon letters reading Happy Birthday Eddie.'],
  ['02-pub-mirrorverse', 'Eddies toasting in a pub hall of mirrors beneath a gilded Happy Birthday Eddie sign.'],
  ['03-riviera', 'Eddies celebrating across a sunny harbour under a Happy Birthday Eddie banner between sailboat masts.'],
  ['04-festival', 'A festival crowd packed with Eddies at the barrier beneath glowing Happy Birthday Eddie letters.'],
  ['05-baroque-ceiling', 'A painted palace ceiling of Eddies on golden clouds with a Happy Birthday Eddie ribbon.'],
  ['06-neon-city', 'Eddies across a rainy neon city crossing under a giant Happy Birthday Eddie billboard.'],
  ['07-cosmic-portals', 'Eddies around a cosmic throne room with portals to his favourite places under Happy Birthday Eddie lettering.'],
  ['08-carnival', 'Eddies all over a night carnival beneath giant illuminated Happy Birthday Eddie marquee letters.'],
  ['09-miniature-world', 'A miniature birthday world full of tiny Eddies beneath Happy Birthday Eddie hillside letters.'],
  ['10-stadium', 'Eddie breaking the finish tape in a cheering stadium beneath a Happy Birthday Eddie banner.'],
];

const HOLD_MS = 10_000;
const list = document.getElementById('slides');
const toggle = document.getElementById('toggle');
const icon = document.getElementById('toggle-icon');
const reduceMotion = matchMedia('(prefers-reduced-motion: reduce)');
const fadeMs = () => reduceMotion.matches ? 0 : 2400;

const first = list.querySelector('.slide');
const slides = [first, ...SLIDES.slice(1).map(([name, alt]) => {
  const slide = first.cloneNode(true);
  const [backdrop, photo] = slide.querySelectorAll('img');
  slide.classList.remove('is-current');
  slide.setAttribute('aria-hidden', 'true');
  backdrop.dataset.src = `images/${name}-640.webp`;
  photo.dataset.src = `images/${name}-1920.webp`;
  photo.dataset.srcset = `images/${name}-1920.webp 1920w, images/${name}-3840.webp 3840w`;
  photo.alt = alt;
  photo.removeAttribute('fetchpriority');
  for (const image of [backdrop, photo]) {
    image.removeAttribute('src');
    image.removeAttribute('srcset');
    image.loading = 'eager';
  }
  list.append(slide);
  return slide;
})];

let current = 0;
let timer = 0;
let cleanup = 0;
let paused = false;
let advancing = false;

function load(slide) {
  for (const image of slide.querySelectorAll('img')) {
    if (image.dataset.srcset) image.srcset = image.dataset.srcset;
    if (image.dataset.src) image.src = image.dataset.src;
    delete image.dataset.src;
    delete image.dataset.srcset;
  }
  const photo = slide.querySelector('.photo');
  if (photo.complete && photo.naturalWidth) return Promise.resolve();
  return photo.decode().catch(() => new Promise((resolve, reject) => {
    if (photo.complete && photo.naturalWidth) resolve();
    else if (photo.complete) reject(new Error(`Could not load ${photo.currentSrc || photo.src}`));
    else {
      photo.addEventListener('load', resolve, { once: true });
      photo.addEventListener('error', reject, { once: true });
    }
  }));
}

function show(nextIndex) {
  const previous = slides[current];
  const next = slides[nextIndex];
  clearTimeout(cleanup);
  slides.forEach(slide => slide.classList.remove('is-previous'));
  previous.classList.replace('is-current', 'is-previous');
  previous.setAttribute('aria-hidden', 'true');
  next.classList.add('is-current');
  next.removeAttribute('aria-hidden');
  current = nextIndex;
  cleanup = setTimeout(() => previous.classList.remove('is-previous'), fadeMs());
  load(slides[(current + 1) % slides.length]).catch(() => {});
}

function schedule() {
  clearTimeout(timer);
  if (paused || document.hidden) return;
  timer = setTimeout(advance, HOLD_MS);
}

async function advance() {
  if (advancing || paused || document.hidden) return;
  advancing = true;
  let nextIndex = (current + 1) % slides.length;
  for (let tries = 0; tries < slides.length - 1; tries++) {
    try {
      await load(slides[nextIndex]);
      if (!paused && !document.hidden) show(nextIndex);
      break;
    } catch {
      nextIndex = (nextIndex + 1) % slides.length;
    }
  }
  advancing = false;
  schedule();
}

function setPaused(value) {
  paused = value;
  document.documentElement.classList.toggle('paused', paused);
  const label = paused ? 'Play slideshow' : 'Pause slideshow';
  toggle.setAttribute('aria-pressed', String(paused));
  toggle.setAttribute('aria-label', label);
  toggle.title = label;
  icon.setAttribute('d', paused ? 'M8 5v14l11-7z' : 'M7 5h3.5v14H7zm6.5 0H17v14h-3.5z');
  if (paused) clearTimeout(timer);
  else schedule();
}

toggle.hidden = false;
toggle.addEventListener('click', () => setPaused(!paused));
document.addEventListener('visibilitychange', schedule);
load(slides[1]).catch(() => {});
schedule();
