/* SARIT search — web worker.
 * Loads texts (data/t/<id>.txt) and their citation data (data/m/<id>.json),
 * builds normalised search keys and runs searches. Several workers run in parallel.
 */
'use strict';
importScripts('translit.js');
const T = self.SaritTranslit;

let BASE = '';
let BUDGET = 40e6;            // characters of cached search keys per worker
let cancelled = {};

// ---------- cache ----------
// Search keys are cached for the current option signature only, and filled until the
// budget is reached (no LRU eviction, which would thrash on sequential scans).
// Original texts are kept only briefly, for building the hits of the current text.
const keyCache = new Map();   // id -> {key, starts: Int32Array, kinds, size}
let keySig = null, keyUsed = 0;
const rawCache = new Map();   // id -> {lines, kinds, size}; small LRU
let rawUsed = 0;
const RAW_BUDGET = 15e6;
const metaCache = new Map();  // id -> meta json

function touch(map, k) { const v = map.get(k); map.delete(k); map.set(k, v); return v; }

async function getRaw(id) {
  if (rawCache.has(id)) return touch(rawCache, id);
  const r = await fetch(BASE + 'data/t/' + encodeURIComponent(id) + '.txt');
  if (!r.ok) throw new Error('cannot load ' + id + ' (' + r.status + ')');
  const txt = await r.text();
  const all = txt.split('\n');
  if (all.length && all[all.length - 1] === '') all.pop();
  const lines = new Array(all.length);
  let kinds = '';
  for (let i = 0; i < all.length; i++) { kinds += all[i][0] || '.'; lines[i] = all[i].slice(1); }
  const v = { lines, kinds, size: txt.length };
  rawCache.set(id, v); rawUsed += v.size;
  while (rawUsed > RAW_BUDGET && rawCache.size > 1) {
    const [k0, v0] = rawCache.entries().next().value;
    rawCache.delete(k0); rawUsed -= v0.size;
  }
  return v;
}

async function getKey(id, opts, sig) {
  if (sig !== keySig) { keyCache.clear(); keyUsed = 0; keySig = sig; }
  if (keyCache.has(id)) return keyCache.get(id);
  const raw = await getRaw(id);
  const n = raw.lines.length;
  const parts = new Array(n);
  const starts = new Int32Array(n + 1);
  let pos = 0;
  for (let i = 0; i < n; i++) {
    const k = T.fastKey(raw.lines[i], opts);
    parts[i] = k; starts[i] = pos; pos += k.length + 1;
  }
  starts[n] = pos;
  const key = parts.join('\n');
  const v = { key, starts, kinds: raw.kinds, size: key.length };
  if (keyUsed + v.size <= BUDGET) { keyCache.set(id, v); keyUsed += v.size; }
  return v;
}

async function getMeta(id) {
  if (metaCache.has(id)) return touch(metaCache, id);
  const r = await fetch(BASE + 'data/m/' + encodeURIComponent(id) + '.json');
  if (!r.ok) throw new Error('cannot load citation data for ' + id);
  const m = await r.json();
  metaCache.set(id, m);
  if (metaCache.size > 30) metaCache.delete(metaCache.keys().next().value);
  return m;
}

function segOf(starts, pos) { // binary search: index i with starts[i] <= pos < starts[i+1]
  let lo = 0, hi = starts.length - 2;
  while (lo < hi) {
    const mid = (lo + hi + 1) >> 1;
    if (starts[mid] <= pos) lo = mid; else hi = mid - 1;
  }
  return lo;
}

// ---------- matching ----------
// Returns Map segIndex -> array of [start,end] in segment-key coordinates
function findMatches(K, q, maxSegs) {
  const { key, starts } = K;
  const res = new Map();
  let total = 0, segsHit = 0;
  const allow = (si) => q.kinds.indexOf(K.kinds[si]) >= 0;
  const add = (si, a, b) => {
    let arr = res.get(si);
    if (!arr) {
      if (!allow(si)) return false;
      segsHit++;
      if (res.size >= maxSegs) { total++; return true; } // count only
      arr = []; res.set(si, arr);
    }
    total++;
    if (arr.length < 50) arr.push([a - starts[si], b - starts[si]]);
    return true;
  };
  if (q.mode === 'all') {
    // every term must occur in the same segment; candidates = segments containing the first term
    const terms = q.terms;
    const res2 = q.termRes ? q.termRes.map(r => new RegExp(r, 'gu')) : null;
    const occ = (str, i) => { // all [start,end] of term i in str
      const out = [];
      if (res2) { const re = res2[i]; re.lastIndex = 0; let m; while ((m = re.exec(str)) !== null) { if (!m[0].length) { re.lastIndex++; continue; } out.push([m.index, m.index + m[0].length]); } }
      else { const t = terms[i]; let x = str.indexOf(t); while (x >= 0) { out.push([x, x + t.length]); x = str.indexOf(t, x + t.length); } }
      return out;
    };
    // candidate passages: those containing the literal part of the first term;
    // regex terms without a literal part (Regex mode with AND) check every passage
    const first = terms[0];
    const nSeg = starts.length - 1;
    let p = first ? key.indexOf(first) : (nSeg ? 0 : -1);
    while (p >= 0) {
      const si = segOf(starts, p);
      const slice = key.slice(starts[si], starts[si + 1] - 1);
      let ok = true;
      const ranges = [];
      for (let i = 0; i < terms.length; i++) {
        const o = occ(slice, i);
        if (!o.length) { ok = false; break; }
        ranges.push(...o);
      }
      if (ok && allow(si)) {
        segsHit++;
        total += ranges.length;
        if (res.size < maxSegs) res.set(si, ranges.sort((a, b) => a[0] - b[0]).slice(0, 50));
      }
      p = first ? key.indexOf(first, starts[si + 1]) : (si + 1 < nSeg ? starts[si + 1] : -1);
    }
    return { res, total, segsHit };
  }
  if (q.mode === 'phrase' && !q.whole) {
    const t = q.terms[0];
    if (!t) return { res, total, segsHit };
    let p = key.indexOf(t);
    while (p >= 0) {
      add(segOf(starts, p), p, p + t.length);
      p = key.indexOf(t, p + t.length);
    }
    return { res, total, segsHit };
  }
  // regex based: regex mode, any-word, whole-word phrase
  const re = new RegExp(q.regex, 'gmu');
  let m, guard = 0;
  while ((m = re.exec(key)) !== null) {
    if (m[0].length === 0) { re.lastIndex++; if (++guard > 1e6) break; continue; }
    const si = segOf(starts, m.index);
    if (m.index + m[0].length <= starts[si + 1] - 1) add(si, m.index, m.index + m[0].length);
  }
  return { res, total, segsHit };
}

// ---------- hit presentation ----------
const DEP = /[ऀ-ःऺ-ॏ॑-ॗॢॣ̀-ͯ᳐-᳿꣠-꣱]/;

function lastMark(marks, typ, off, dflt) {
  let v = dflt;
  if (marks) for (const m of marks) { if (m[0] > off) break; if (m[1] === typ) v = m[2]; }
  return v;
}

function buildHit(id, si, ranges, raw, meta, opts, ctxChars) {
  const text = raw.lines[si];
  const N = T.normalize(text, opts, true);
  const map = N.map;
  const hl = [];
  for (const [a, b] of ranges) {
    if (a >= map.length) continue;
    let s = map[a], e = map[Math.min(b, map.length) - 1] + 1;
    while (e < text.length && DEP.test(text[e])) e++;
    if (hl.length && s < hl[hl.length - 1][1]) { hl[hl.length - 1][1] = Math.max(e, hl[hl.length - 1][1]); continue; }
    hl.push([s, e]);
  }
  const mseg = meta.segs[si] || [0, '', '', 0];
  const marks = mseg[4];
  const first = hl.length ? hl[0][0] : 0;
  const ref = lastMark(marks, 'r', first, mseg[1] || '');
  const loc = lastMark(marks, 'p', first, mseg[2] || '');
  // snippet window
  let from = 0, to = text.length;
  if (text.length > ctxChars * 2 + 200 && hl.length) {
    from = Math.max(0, hl[0][0] - ctxChars);
    to = Math.min(text.length, hl[hl.length - 1][1] + ctxChars);
    if (to - from > ctxChars * 4 + 400) to = Math.min(text.length, hl[0][1] + ctxChars);
    // snap to spaces
    if (from > 0) { const sp = text.indexOf(' ', from); if (sp >= 0 && sp < hl[0][0]) from = sp + 1; }
    if (to < text.length) { const sp = text.lastIndexOf(' ', to); const lastHl = hl.filter(h => h[0] < to).pop(); if (sp > (lastHl ? lastHl[1] : 0)) to = sp; }
  }
  const snippet = text.slice(from, to);
  const hls = hl.filter(h => h[0] >= from && h[1] <= to).map(h => [h[0] - from, h[1] - from]);
  return {
    text: id, seg: si, kind: raw.kinds[si], ref, loc, ctx: meta.ctx[mseg[0]] || '', line: mseg[3],
    segRef: mseg[1] || '', snippet, hls, pre: from > 0, post: to < text.length, n: ranges.length
  };
}

// ---------- message handling ----------
self.onmessage = async (ev) => {
  const d = ev.data;
  if (d.type === 'init') { BASE = d.base; BUDGET = d.budget || BUDGET; return; }
  if (d.type === 'cancel') { cancelled[d.sid] = true; return; }
  if (d.type === 'segment') { // fetch one segment (for copy / context)
    try {
      const raw = await getRaw(d.text), meta = await getMeta(d.text);
      self.postMessage({ type: 'segment', rid: d.rid, text: raw.lines[d.seg], meta: meta.segs[d.seg], ctx: meta.ctx });
    } catch (e) { self.postMessage({ type: 'segment', rid: d.rid, error: String(e) }); }
    return;
  }
  if (d.type !== 'search') return;
  const { sid, texts, q, opts, sig, maxSegsPerText, ctxChars } = d;
  for (const id of texts) {
    if (cancelled[sid]) break;
    try {
      const K = await getKey(id, opts, sig);
      if (cancelled[sid]) break;
      const { res, total, segsHit } = findMatches(K, q, maxSegsPerText);
      let hits = [];
      if (res.size) {
        const raw = await getRaw(id);
        const meta = await getMeta(id);
        for (const [si, ranges] of res) hits.push(buildHit(id, si, ranges, raw, meta, opts, ctxChars));
        hits.sort((a, b) => a.seg - b.seg);
      }
      self.postMessage({ type: 'result', sid, text: id, total, segsHit, hits, truncated: segsHit > res.size });
    } catch (e) {
      let msg = String(e && e.message || e);
      if (/recursion|call stack|stack overflow|backtrack/i.test(msg))
        msg = 'the pattern is too complex for a long passage in this text (' + msg + '). Make it more specific, e.g. avoid .* in the middle of the pattern.';
      self.postMessage({ type: 'result', sid, text: id, total: 0, segsHit: 0, hits: [], error: msg });
    }
  }
  self.postMessage({ type: 'done', sid });
  delete cancelled[sid];
};
