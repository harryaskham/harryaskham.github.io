const audio = document.querySelector('#audio');
const player = document.querySelector('#player');
const toggle = document.querySelector('#toggle');
const label = document.querySelector('#play-label');
const seek = document.querySelector('#seek');
const elapsed = document.querySelector('#elapsed');
const duration = document.querySelector('#duration');
const feedback = document.querySelector('.feedback');
const status = document.querySelector('#status');
const download = document.querySelector('#download');
let attempt = 0;

function time(seconds) {
  if (!Number.isFinite(seconds)) return '—:——';
  const whole = Math.max(0, Math.floor(seconds));
  return `${Math.floor(whole / 60)}:${String(whole % 60).padStart(2, '0')}`;
}

function message(text = '', failed = false) {
  status.textContent = text;
  download.hidden = !failed;
  feedback.hidden = !text;
}

function state() {
  const playing = !audio.paused;
  const name = playing ? 'Pause' : 'Play';
  toggle.dataset.playing = String(playing);
  toggle.setAttribute('aria-label', name);
  label.textContent = name;
  if ('mediaSession' in navigator) navigator.mediaSession.playbackState = playing ? 'playing' : 'paused';
}

function position() {
  const ready = Number.isFinite(audio.duration) && audio.duration > 0;
  seek.disabled = !ready;
  seek.max = ready ? audio.duration : 100;
  seek.value = audio.currentTime;
  seek.style.setProperty('--progress', `${ready ? audio.currentTime / audio.duration * 100 : 0}%`);
  seek.setAttribute('aria-valuetext', `${time(audio.currentTime)} of ${time(audio.duration)}`);
  elapsed.textContent = time(audio.currentTime);
  duration.textContent = time(audio.duration);
}

async function play() {
  const current = ++attempt;
  message();
  if (audio.error) audio.load();
  try {
    // Called directly from a gesture; do not await a fetch before play (iOS).
    await audio.play();
    if (current === attempt) message();
  } catch (error) {
    if (current !== attempt || error.name === 'AbortError') return;
    message(error.name === 'NotAllowedError' ? 'Press Play to start the audio.' : 'Audio unavailable. Try Play again.', true);
  }
  state();
}

function pause() {
  attempt++;
  audio.pause();
  message();
  state();
}

toggle.addEventListener('click', () => audio.paused ? void play() : pause());
seek.addEventListener('input', () => {
  if (!seek.disabled) audio.currentTime = Number(seek.value);
  position();
});
for (const event of ['loadedmetadata', 'durationchange', 'timeupdate', 'seeked', 'emptied']) {
  audio.addEventListener(event, position);
}
for (const event of ['play', 'pause']) audio.addEventListener(event, state);
audio.addEventListener('playing', () => { message(); state(); });
audio.addEventListener('waiting', () => { if (!audio.paused) message('Loading…'); });
audio.addEventListener('error', () => {
  pause();
  message('Audio unavailable. Try Play again.', true);
});
window.addEventListener('pagehide', pause);

// Keep a working native player when JavaScript is disabled or fails to load.
audio.hidden = true;
player.hidden = false;
position();
state();
if (audio.error) message('Audio unavailable. Try Play again.', true);

if ('mediaSession' in navigator && 'MediaMetadata' in window) {
  navigator.mediaSession.metadata = new MediaMetadata({
    title: 'All-Star Pars',
    artist: 'Tempa T',
    artwork: [{ src: new URL('artwork.png', document.baseURI).href, sizes: '1024x1024', type: 'image/png' }],
  });
  navigator.mediaSession.setActionHandler('play', () => void play());
  navigator.mediaSession.setActionHandler('pause', pause);
}
