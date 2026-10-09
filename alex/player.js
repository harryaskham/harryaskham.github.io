const button = document.querySelector('#play');
const audio = document.querySelector('#audio');
const label = document.querySelector('#label');
const icon = document.querySelector('#icon');
const status = document.querySelector('#status');
let tracks = [], bag = [], last = '', active = false, generation = 0;

function state(message = '', error = false) {
  label.textContent = active ? 'Stop' : 'Play';
  button.setAttribute('aria-label', label.textContent);
  button.setAttribute('aria-pressed', String(active));
  icon.setAttribute('d', active ? 'M18 18H46V46H18Z' : 'M24 14 50 32 24 50Z');
  status.textContent = message;
  status.classList.toggle('error', error);
}
function next() {
  if (!bag.length) {
    bag = [...tracks];
    for (let i = bag.length - 1; i > 0; i--) {
      const j = Math.floor(Math.random() * (i + 1));
      [bag[i], bag[j]] = [bag[j], bag[i]];
    }
    if (bag.length > 1 && bag.at(-1) === last) [bag[0], bag[bag.length - 1]] = [bag.at(-1), bag[0]];
  }
  last = bag.pop();
  return last;
}
function stop(message = '', error = false) {
  active = false;
  generation++;
  audio.pause();
  audio.removeAttribute('src');
  audio.load();
  state(message, error);
}
function playNext() {
  const current = ++generation;
  audio.src = new URL(next(), document.baseURI).href;
  state('Loading…');
  audio.play().catch(() => {
    if (current === generation && active) stop('Couldn’t play. Try again.', true);
  });
}
async function load() {
  button.disabled = true;
  state('Loading…');
  try {
    const response = await fetch('tracks.json', { cache: 'no-cache', signal: AbortSignal.timeout(10000) });
    if (!response.ok) throw Error('unavailable');
    tracks = await response.json();
    if (!Array.isArray(tracks) || tracks.length < 1 || tracks.length > 100 ||
        tracks.some(t => typeof t !== 'string' || !/^audio\/[a-f0-9]{16}\.mp3$/.test(t))) throw Error('invalid');
    state();
  } catch {
    tracks = [];
    state('Unavailable. Try again.', true);
  } finally { button.disabled = false; }
}
button.addEventListener('click', () => {
  if (active) return stop();
  if (!tracks.length) return void load();
  active = true;
  playNext();
});
audio.addEventListener('playing', () => { if (active) state(); });
audio.addEventListener('waiting', () => { if (active) state('Loading…'); });
audio.addEventListener('ended', () => { if (active) playNext(); });
audio.addEventListener('error', () => { if (active) stop('Couldn’t play. Try again.', true); });
window.addEventListener('pagehide', () => stop());
void load();
