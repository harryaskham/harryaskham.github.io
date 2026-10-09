// Real-browser check for /jay/: on-device model load, upload transcription,
// live dictation through a fake microphone, playback, search, persistence and
// phone layout. Audio fixtures are public MedASR test clips fetched to /tmp.
//
//   JAY_URL=http://127.0.0.1:4000/jay/ node _tests/jay-browser.mjs
//   JAY_MEDASR=1 …   also download MedASR (~108 MB) and check clinical output
//
// Env: CHROMIUM (browser binary), PLAYWRIGHT_CORE (module path).
import fs from "node:fs";
import { createRequire } from "node:module";

const URL_ = process.env.JAY_URL || "http://127.0.0.1:4000/jay/";
const require = createRequire(import.meta.url);
const pw = require(process.env.PLAYWRIGHT_CORE || "playwright-core");
const TMP = "/tmp/jay-browser-check";
const CLIP = "https://huggingface.co/csukuangfj/sherpa-onnx-medasr-ctc-en-int8-2025-12-25/resolve/main/test_wavs/";
fs.mkdirSync(TMP, { recursive: true });

function readWav(buf) {
  const dv = new DataView(buf.buffer, buf.byteOffset, buf.byteLength);
  let o = 12, rate = 16000, ch = 1, data;
  while (o < buf.length) {
    const id = buf.toString("ascii", o, o + 4), n = dv.getUint32(o + 4, true);
    if (id === "fmt ") { ch = dv.getUint16(o + 10, true); rate = dv.getUint32(o + 12, true); }
    if (id === "data") data = new Int16Array(buf.buffer.slice(buf.byteOffset + o + 8, buf.byteOffset + o + 8 + n));
    o += 8 + n + (n & 1);
  }
  const mono = new Float32Array(data.length / ch);
  for (let i = 0; i < mono.length; i++) mono[i] = data[i * ch] / 32768;
  return { rate, pcm: mono };
}
function resample({ rate, pcm }, to) {
  const out = new Float32Array(Math.floor((pcm.length * to) / rate));
  for (let i = 0; i < out.length; i++) {
    const x = (i * rate) / to, j = Math.floor(x), f = x - j;
    out[i] = (pcm[j] || 0) * (1 - f) + (pcm[j + 1] || 0) * f;
  }
  return out;
}
function writeWav(path, f32, rate) {
  const b = Buffer.alloc(44 + f32.length * 2);
  b.write("RIFF", 0); b.writeUInt32LE(36 + f32.length * 2, 4); b.write("WAVEfmt ", 8);
  b.writeUInt32LE(16, 16); b.writeUInt16LE(1, 20); b.writeUInt16LE(1, 22); b.writeUInt32LE(rate, 24);
  b.writeUInt32LE(rate * 2, 28); b.writeUInt16LE(2, 32); b.writeUInt16LE(16, 34); b.write("data", 36); b.writeUInt32LE(f32.length * 2, 40);
  for (let i = 0; i < f32.length; i++) b.writeInt16LE(Math.max(-32768, Math.min(32767, Math.round(f32[i] * 32767))), 44 + i * 2);
  fs.writeFileSync(path, b);
}
async function clip(name) {
  const p = `${TMP}/${name}`;
  if (!fs.existsSync(p)) fs.writeFileSync(p, Buffer.from(await (await fetch(CLIP + name)).arrayBuffer()));
  return readWav(fs.readFileSync(p));
}

const vitals = await clip("5.wav"), biopsy = await clip("1.wav"), radiology = `${TMP}/0.wav`;
await clip("0.wav");
const sil = (s, r) => new Float32Array(Math.round(s * r));
const join = (...xs) => { const o = new Float32Array(xs.reduce((n, x) => n + x.length, 0)); let k = 0; for (const x of xs) { o.set(x, k); k += x.length; } return o; };
writeWav(`${TMP}/upload.wav`, join(resample(vitals, 16000), sil(1.5, 16000), resample(biopsy, 16000)), 16000);
writeWav(`${TMP}/mic48.wav`, join(sil(1.5, 48000), resample(vitals, 48000), sil(1.5, 48000), resample(biopsy, 48000), sil(4, 48000)), 48000);

const ok = (c, m) => { if (!c) throw new Error("FAIL: " + m); console.log("✓", m); };
const browser = await pw.chromium.launch({
  executablePath: process.env.CHROMIUM || undefined,
  // Headless Chromium's fake audio sink lets the AudioContext clock stall under CPU load;
  // a larger buffer keeps it closer to realtime. Jay recovers from stalls via MediaRecorder.
  args: ["--use-fake-ui-for-media-stream", "--use-fake-device-for-media-stream", `--use-file-for-fake-audio-capture=${TMP}/mic48.wav%noloop`, "--audio-buffer-size=2048"],
});
const ctx = await browser.newContext({ viewport: { width: 1360, height: 860 }, permissions: ["microphone"] });
const page = await ctx.newPage();
const errors = [];
page.on("pageerror", (e) => errors.push(e.message));
try {
  let capture = null;
page.on("console", (m) => { const t = m.text(); if (t.startsWith("[jay] capture")) capture = JSON.parse(t.slice(14)); });
await page.addInitScript(() => localStorage.setItem("jay.debug", "1"));
await page.goto(URL_);
  await page.waitForFunction(() => /ready/.test(document.querySelector("#loadline")?.textContent || ""), null, { timeout: 120000 });
  ok(await page.evaluate(() => crossOriginIsolated), "cross-origin isolated via service worker (WASM threads)");
  ok(/thread/.test(await page.textContent("#loadline")), "built-in model ready: " + (await page.textContent("#loadline")));

  await page.setInputFiles("#file", `${TMP}/upload.wav`);
  await page.waitForFunction(() => document.querySelector(".turn .chip")?.textContent.includes("Moonshine"), null, { timeout: 120000 });
  const up = await page.textContent(".turn .text");
  ok(/temperature is 37\.2 degrees/i.test(up) && /biopsy/i.test(up), "upload transcribed on device");
  ok((await page.$$(".turn .text p")).length >= 2, "pause splits paragraphs");

  await page.keyboard.press("r");
  let partial = false;
  for (let i = 0; i < 40 && !partial; i++) { await page.waitForTimeout(500); partial = !!(await page.$(".turn.live .partial")); }
  await page.waitForTimeout(36000); // fixture is ~34 s; stop after it ends
  await page.keyboard.press("r");
  await page.waitForTimeout(1500);
  const clock = capture ? capture.seen / capture.rate / capture.wall : 1;
  if (!partial && clock < 0.7) console.log(`! no live partial — audio clock ran at ${clock.toFixed(2)}× realtime (headless sink stalled); recovery covers this`);
  else ok(partial, `live partial transcript while speaking (audio clock ${clock.toFixed(2)}×)`);
  await page.waitForFunction(() => !document.querySelector(".turn.live") && [...document.querySelectorAll(".turn .chip")].every((c) => !/transcrib|live|queued/.test(c.textContent)), null, { timeout: 60000 });
  const dict = await page.evaluate(() => [...document.querySelectorAll(".turn")].at(-1).querySelector(".text").textContent);
  ok(/heart rate/i.test(dict) && /oste\w+ is a condition in which bones become weak/i.test(dict), "live dictation finalised through the tail: " + dict.slice(-90));
  const live = await page.evaluate(() => { const c = document.querySelector("#live-sono"); return c.width > 0; });
  ok(live, "live sonogram canvas sized");

  await page.click(".turn .play");
  await page.waitForTimeout(900);
  ok(await page.evaluate(() => !document.querySelector("#player").paused), "playback from local audio");
  await page.keyboard.press(" ");

  await page.fill("#search", "osteoporosis");
  await page.waitForTimeout(250);
  ok((await page.$$("#sessions .sess")).length === 1 && (await page.$$(".turn mark")).length >= 1, "search finds and highlights");
  await page.fill("#search", "");

  if (process.env.JAY_MEDASR) {
    await page.click("#model-chip"); await page.click('#menu button:has-text("MedASR")');
    await page.waitForFunction(() => document.querySelector("#model-chip .dot")?.classList.contains("ready"), null, { timeout: 600000 });
    await page.setInputFiles("#file", radiology);
    await page.waitForFunction(() => [...document.querySelectorAll(".turn .chip")].some((c) => c.textContent.includes("MedASR")), null, { timeout: 180000 });
    const med = await page.evaluate(() => [...document.querySelectorAll(".turn")].at(-1).querySelector(".text").innerText);
    ok(/impression:/i.test(med) && /^findings:/im.test(med) && /pneumothorax/i.test(med), "MedASR clinical sections: " + med.split("\n").filter((l) => /:/.test(l)).map((l) => l.split(":")[0]).join(" · "));
  }

  await page.reload();
  await page.waitForSelector(".turn");
  ok((await page.$$(".turn")).length >= 2, "history persists across reload");

  await page.setViewportSize({ width: 390, height: 844 });
  await page.waitForTimeout(400);
  ok(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth && document.querySelector("#page").getBoundingClientRect().right <= innerWidth + 1), "no horizontal overflow on phone");
  ok(!errors.length, "no page errors" + (errors.length ? ": " + errors.join("; ") : ""));
} finally {
  await browser.close();
}
