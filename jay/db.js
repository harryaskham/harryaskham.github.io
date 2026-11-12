// jay-build:7e8cfe1543
// Jay · IndexedDB persistence: sessions, turns, audio blobs, in-flight PCM.
const NAME = "jay", VERSION = 1;
let dbp;

export function open() {
  return (dbp ||= new Promise((resolve, reject) => {
    const r = indexedDB.open(NAME, VERSION);
    r.onupgradeneeded = () => {
      const d = r.result;
      d.createObjectStore("sessions", { keyPath: "id" });
      d.createObjectStore("turns", { keyPath: "id" }).createIndex("session", "sessionId");
      d.createObjectStore("audio");
      d.createObjectStore("pcm", { autoIncrement: true }).createIndex("turn", "turnId");
    };
    r.onsuccess = () => resolve(r.result);
    r.onerror = () => reject(r.error);
    r.onblocked = () => reject(new Error("Database is open in another tab"));
  }));
}

async function tx(store, mode, fn) {
  const d = await open();
  return new Promise((resolve, reject) => {
    const t = d.transaction(store, mode);
    const s = t.objectStore(store);
    let result;
    const req = fn(s);
    if (req && "onsuccess" in req) req.onsuccess = () => (result = req.result);
    t.oncomplete = () => resolve(result);
    t.onerror = t.onabort = () => reject(t.error);
  });
}

// Some engines (older Safari/WebKit) can't store Blobs in IndexedDB. Probe once and,
// if needed, store audio as {buf, type, name} and rebuild Blobs on read.
let blobOk;
async function blobsSupported() {
  if (blobOk !== undefined) return blobOk;
  const d = await open();
  blobOk = await new Promise((resolve) => {
    try {
      const t = d.transaction("audio", "readwrite");
      t.objectStore("audio").put(new Blob([new Uint8Array(1)]), "__probe");
      t.objectStore("audio").delete("__probe");
      t.oncomplete = () => resolve(true);
      t.onerror = t.onabort = (e) => { e?.preventDefault?.(); resolve(false); };
    } catch { resolve(false); }
  });
  return blobOk;
}
const isPacked = (v) => v && v.__jay === 1 && v.buf instanceof ArrayBuffer;
const unpack = (v) => (isPacked(v) ? (v.name ? new File([v.buf], v.name, { type: v.type }) : new Blob([v.buf], { type: v.type })) : v);
async function pack(store, v) {
  if (store !== "audio" || !(v instanceof Blob) || (await blobsSupported())) return v;
  return { __jay: 1, buf: await v.arrayBuffer(), type: v.type, name: v.name || "" };
}

export const all = async (store) => (await tx(store, "readonly", (s) => s.getAll())).map(unpack);
export const get = async (store, key) => unpack(await tx(store, "readonly", (s) => s.get(key)));
export const put = async (store, val, key) => {
  const v = await pack(store, val);
  return tx(store, "readwrite", (s) => (key === undefined ? s.put(v) : s.put(v, key)));
};
export const del = (store, key) => tx(store, "readwrite", (s) => s.delete(key));
export const clear = (store) => tx(store, "readwrite", (s) => s.clear());
export const byIndex = (store, index, key) => tx(store, "readonly", (s) => s.index(index).getAll(key));
export const keysByIndex = (store, index, key) => tx(store, "readonly", (s) => s.index(index).getAllKeys(key));
export async function delByIndex(store, index, key) {
  const keys = await keysByIndex(store, index, key);
  if (!keys.length) return;
  await tx(store, "readwrite", (s) => { keys.forEach((k) => s.delete(k)); });
}
export async function putMany(store, vals) {
  if (!vals.length) return;
  await tx(store, "readwrite", (s) => { vals.forEach((v) => s.put(v)); });
}
