/* SARIT search — script-neutral normalisation and transliteration.
 *
 * Every text and every query is reduced to the same "search key":
 *   Devanagari → IAST, lower-case, ISO-15919 variants → IAST (ṁ→ṃ, r̥→ṛ, ē→e …),
 *   accents removed, daṇḍas unified (/ | । → |, // || ॥ → ||), avagraha ऽ ’ → '.
 * Optional transforms:
 *   nasal  – class nasal before a consonant of its class counts as ṃ (saṅkalpa = saṃkalpa)
 *   noDiac – all diacritics removed (ā→a, ṣ→s, ṃ→m …)
 *   noSep  – spaces, hyphens, dots, '+', '_' and avagraha ignored (word division ignored)
 *
 * normalize(str, opts, true) is the reference implementation and returns a map from
 * key positions back to source positions (used for highlighting);
 * fastKey(str, opts) returns the identical key, faster, without the map.
 */
(function (G) {
  'use strict';

  // ---------------- Devanagari tables ----------------
  const CONS = {
    'क': 'k', 'ख': 'kh', 'ग': 'g', 'घ': 'gh', 'ङ': 'ṅ', 'च': 'c', 'छ': 'ch', 'ज': 'j', 'झ': 'jh', 'ञ': 'ñ',
    'ट': 'ṭ', 'ठ': 'ṭh', 'ड': 'ḍ', 'ढ': 'ḍh', 'ण': 'ṇ', 'त': 't', 'थ': 'th', 'द': 'd', 'ध': 'dh', 'न': 'n',
    '\u0929': 'ṉ', 'प': 'p', 'फ': 'ph', 'ब': 'b', 'भ': 'bh', 'म': 'm', 'य': 'y', 'र': 'r', '\u0931': 'ṟ', 'ल': 'l',
    'ळ': 'ḷ', '\u0934': 'ḻ', 'व': 'v', 'श': 'ś', 'ष': 'ṣ', 'स': 's', 'ह': 'h',
    '\u0958': 'q', '\u0959': 'ḵẖ', '\u095A': 'ġ', '\u095B': 'z', '\u095C': 'ṛ', '\u095D': 'ṛh', '\u095E': 'f', '\u095F': 'ẏ'
  };
  const NUKTA = { 'क': 'q', 'ख': 'ḵẖ', 'ग': 'ġ', 'ज': 'z', 'ड': 'ṛ', 'ढ': 'ṛh', 'फ': 'f', 'य': 'ẏ', 'न': 'ṉ', 'र': 'ṟ', 'ळ': 'ḻ' };
  const MATRA = {
    'ा': 'ā', 'ि': 'i', 'ी': 'ī', 'ु': 'u', 'ू': 'ū', 'ृ': 'ṛ', 'ॄ': 'ṝ', 'ॢ': 'ḷ', 'ॣ': 'ḹ',
    'े': 'e', 'ै': 'ai', 'ो': 'o', 'ौ': 'au', 'ॅ': 'ê', 'ॉ': 'ô', 'ॆ': 'e', 'ॊ': 'o', 'ॏ': 'aw'
  };
  const OTHER = {
    'अ': 'a', 'आ': 'ā', 'इ': 'i', 'ई': 'ī', 'उ': 'u', 'ऊ': 'ū', 'ऋ': 'ṛ', 'ॠ': 'ṝ', 'ऌ': 'ḷ', 'ॡ': 'ḹ',
    'ए': 'e', 'ऐ': 'ai', 'ओ': 'o', 'औ': 'au', 'ऍ': 'ê', 'ऑ': 'ô', 'ऎ': 'e', 'ऒ': 'o', 'ॲ': 'ê',
    'ं': 'ṃ', 'ः': 'ḥ', 'ँ': 'm̐', 'ऽ': "'", 'ॐ': 'oṃ', '।': '|', '॥': '||', '॰': '.',
    '०': '0', '१': '1', '२': '2', '३': '3', '४': '4', '५': '5', '६': '6', '७': '7', '८': '8', '९': '9',
    'ᳲ': 'ḫ', 'ᳵ': 'ḫ', 'ᳳ': 'ḫ', 'ऀ': 'm̐'
  };
  const VIRAMA = '्', NUKTA_SIGN = '़';
  const isDevaCode = c => (c >= 0x900 && c <= 0x97F) || (c >= 0xA8E0 && c <= 0xA8FF) || (c >= 0x1CD0 && c <= 0x1CFF);

  // ---------------- Latin tables ----------------
  // single-char substitutions applied after lower-casing
  const LAT = {
    'ṁ': 'ṃ', 'ē': 'e', 'ō': 'o', 'ḿ': 'ṃ',
    '/': '|', '’': "'", '‘': "'", 'ʼ': "'", 'ʻ': "'", '′': "'",
    '\u2028': ' ', '\u00A0': ' ', '\t': ' ', '\u2009': ' ', '\u202F': ' ', '\n': ' ', '\r': ' ',
    '\u200C': '', '\u200D': '', '\u00AD': '', '\uFEFF': '', '\u200B': '',
    '‐': '-', '‑': '-',
    // precomposed accented vowels (Vedic/Greek-style accents) → plain vowel
    'á': 'a', 'à': 'a', 'é': 'e', 'è': 'e', 'í': 'i', 'ì': 'i', 'ó': 'o', 'ò': 'o', 'ú': 'u', 'ù': 'u',
    'â': 'a', 'î': 'i', 'û': 'u'
  };
  const LAT_RE = new RegExp('[' + Object.keys(LAT).map(esc1).join('') + ']', 'g');
  function esc1(c) { return c.replace(/[\\\]\[\-^\/]/g, '\\$&').replace('\n', '\\n').replace('\r', '\\r').replace('\t', '\\t').replace('\u2028', '\\u2028'); }
  const ACCENTS = { '\u0300': 1, '\u0301': 1, '\u0302': 1, '\u030D': 1, '\u030E': 1, '\u0951': 1, '\u0952': 1, '\u0953': 1, '\u0954': 1 };

  const NASAL_RE = /ṅ(?=[kg])|ñ(?=[cj])|ṇ(?=[ṭḍ])|n(?=[td])|m(?=[pb])/g;
  const NASAL_NEXT = { 'ṅ': 'kg', 'ñ': 'cj', 'ṇ': 'ṭḍ', 'n': 'td', 'm': 'pb' };
  const SEP_RE = /[ \-.+_']/g;
  const SH_RE = /sh/g;
  const SEP = { ' ': 1, '-': 1, '.': 1, '+': 1, '_': 1, "'": 1 };

  const stripCache = Object.create(null);
  function stripChar(ch) {
    let r = stripCache[ch];
    if (r !== undefined) return r;
    const code = ch.charCodeAt(0);
    if (code >= 0x300 && code <= 0x36F) r = '';
    else r = ch.normalize('NFD').replace(/[\u0300-\u036F]/g, '');
    stripCache[ch] = r;
    return r;
  }
  const NONASCII_RE = /[^\x00-\x7F]/g;

  function latChar(ch) {
    const lo = ch.toLowerCase();
    if (lo.length !== 1) return lo;
    const t = LAT[lo];
    return t === undefined ? lo : t;
  }

  // combine a combining mark with the preceding output character
  function combine(prev, mark) {
    if (ACCENTS[mark]) return { drop: true };
    if (mark === '\u0325') { // ring below: r̥ l̥
      if (prev === 'r') return { rep: 'ṛ' };
      if (prev === 'l') return { rep: 'ḷ' };
    }
    if (mark === '\u0304') { // macron
      if (prev === 'ṛ') return { rep: 'ṝ' };
      if (prev === 'ḷ') return { rep: 'ḹ' };
    }
    if (prev) {
      const c = (prev + mark).normalize('NFC');
      if (c.length === 1) {
        const l = latChar(c);
        return { rep: l };
      }
    }
    return { keep: true };
  }

  // ---------------- reference implementation ----------------
  function normalize(s, o, wantMap) {
    o = o || {};
    const out = [], map = [];
    const n = s.length;
    const push = (str, src) => { for (let k = 0; k < str.length; k++) { out.push(str[k]); map.push(src); } };
    for (let i = 0; i < n; i++) {
      const c = s[i], code = s.charCodeAt(i);
      if (isDevaCode(code)) {
        let base = CONS[c];
        if (base !== undefined) {
          let j = i + 1;
          if (s[j] === NUKTA_SIGN) { base = NUKTA[c] || base; j++; }
          push(base, i);
          const nx = s[j];
          if (nx === VIRAMA) { i = j; continue; }
          const m = nx !== undefined ? MATRA[nx] : undefined;
          if (m !== undefined) { push(m, j); i = j; continue; }
          push('a', i); i = j - 1; continue;
        }
        const t = OTHER[c];
        if (t !== undefined) push(t, i);
        else if (MATRA[c] !== undefined) push(MATRA[c], i); // stray matra
        // everything else (accents, virama alone, nukta alone) is dropped
        continue;
      }
      if (code >= 0x300 && code <= 0x36F) {
        const prev = out.length ? out[out.length - 1] : '';
        const r = combine(prev, c);
        if (r.drop) continue;
        if (r.rep !== undefined) { out[out.length - 1] = r.rep; continue; }
        push(c, i);
        continue;
      }
      push(latChar(c), i);
    }
    let K = out, M = map;
    if (o.nasal) {
      for (let k = 0; k < K.length - 1; k++) {
        const nn = NASAL_NEXT[K[k]];
        if (nn && nn.indexOf(K[k + 1]) >= 0) K[k] = 'ṃ';
      }
    }
    if (o.noDiac || o.noSep) {
      const K2 = [], M2 = [];
      for (let k = 0; k < K.length; k++) {
        let ch = K[k];
        if (o.noDiac && ch.charCodeAt(0) > 0x7F) ch = stripChar(ch);
        if (o.noSep && SEP[ch]) ch = '';
        for (let q = 0; q < ch.length; q++) { K2.push(ch[q]); M2.push(M[k]); }
      }
      K = K2; M = M2;
    }
    if (o.noDiac) { // loose ASCII: "sh" = s (so krishna/shiva-style typing finds ś, ṣ)
      const K3 = [], M3 = [];
      for (let k = 0; k < K.length; k++) {
        K3.push(K[k]); M3.push(M[k]);
        if (K[k] === 's' && K[k + 1] === 'h') k++;
      }
      K = K3; M = M3;
    }
    const key = K.join('');
    return wantMap ? { key, map: M } : key;
  }

  // ---------------- fast implementation (same result) ----------------
  const SLOW_RE = /[\u0300-\u036F\u0900-\u097F\uA8E0-\uA8FF\u1CD0-\u1CFF]/;
  function latReplace(ch) { return LAT[ch]; }
  const COMB_RE = /[\u0300-\u036F]/;
  const DEVA_RE = /[\u0900-\u097F\uA8E0-\uA8FF\u1CD0-\u1CFF]/;
  // lookup arrays indexed by (charCode - 0x900)
  const A_CONS = new Array(128).fill(null), A_NUK = new Array(128).fill(null),
    A_MAT = new Array(128).fill(null), A_OTH = new Array(128).fill(null);
  for (const k in CONS) if (k.length === 1) A_CONS[k.charCodeAt(0) - 0x900] = CONS[k];
  for (const k in NUKTA) A_NUK[k.charCodeAt(0) - 0x900] = NUKTA[k];
  for (const k in MATRA) A_MAT[k.charCodeAt(0) - 0x900] = MATRA[k];
  for (const k in OTHER) { const c = k.charCodeAt(0); if (c >= 0x900 && c < 0x980) A_OTH[c - 0x900] = OTHER[k]; }
  // Devanagari → base key for strings without combining marks (same result as normalize()).
  function devaFast(s) {
    let out = '';
    const n = s.length;
    let run = 0; // start of pending non-Devanagari run
    for (let i = 0; i < n; i++) {
      const code = s.charCodeAt(i);
      const isD = (code >= 0x900 && code <= 0x97F);
      const isX = !isD && ((code >= 0xA8E0 && code <= 0xA8FF) || (code >= 0x1CD0 && code <= 0x1CFF));
      if (!isD && !isX) continue;
      if (run < i) out += latRun(s.slice(run, i));
      run = i + 1;
      if (isX) { const t = OTHER[s[i]]; if (t !== undefined) out += t; continue; }
      const d = code - 0x900;
      let base = A_CONS[d];
      if (base !== null) {
        let j = i + 1;
        if (s.charCodeAt(j) === 0x93C) { const nb = A_NUK[d]; if (nb !== null) base = nb; j++; }
        out += base;
        const nc = s.charCodeAt(j);
        if (nc === 0x94D) { i = j; run = i + 1; continue; }
        if (nc >= 0x900 && nc < 0x980) {
          const m = A_MAT[nc - 0x900];
          if (m !== null) { out += m; i = j; run = i + 1; continue; }
        }
        out += 'a'; i = j - 1; run = i + 1; continue;
      }
      const t = A_OTH[d];
      if (t !== null) out += t;
      else { const m = A_MAT[d]; if (m !== null) out += m; }
    }
    if (run < n) out += latRun(s.slice(run));
    return out;
  }
  function latRun(r) {
    const lo = r.toLowerCase();
    if (lo.length !== r.length) return normalize(r, null, false);
    return lo.replace(LAT_RE, latReplace);
  }
  function fastKey(s, o) {
    let k;
    if (COMB_RE.test(s)) k = normalize(s, null, false);
    else if (DEVA_RE.test(s)) k = devaFast(s);
    else {
      k = s.toLowerCase();
      if (k.length !== s.length) k = normalize(s, null, false); // exotic case mappings
      else k = k.replace(LAT_RE, latReplace);
    }
    if (o) {
      if (o.nasal) k = k.replace(NASAL_RE, 'ṃ');
      if (o.noDiac) k = k.replace(NONASCII_RE, stripChar);
      if (o.noSep) k = k.replace(SEP_RE, '');
      if (o.noDiac) k = k.replace(SH_RE, 's');
    }
    return k;
  }

  // ---------------- input schemes for queries ----------------
  function fromHK(s) {
    const T = [['lRR', 'ḹ'], ['lR', 'ḷ'], ['RR', 'ṝ'], ['R', 'ṛ'], ['A', 'ā'], ['I', 'ī'], ['U', 'ū'], ['M', 'ṃ'], ['H', 'ḥ'],
      ['G', 'ṅ'], ['J', 'ñ'], ['T', 'ṭ'], ['D', 'ḍ'], ['N', 'ṇ'], ['z', 'ś'], ['S', 'ṣ']];
    return rep(s, T);
  }
  function fromVelthuis(s) {
    const T = [['.rr', 'ṝ'], ['.r', 'ṛ'], ['.ll', 'ḹ'], ['.l', 'ḷ'], ['aa', 'ā'], ['ii', 'ī'], ['uu', 'ū'], ['.m', 'ṃ'], ['.h', 'ḥ'],
      ['"n', 'ṅ'], ['~n', 'ñ'], ['.t', 'ṭ'], ['.d', 'ḍ'], ['.n', 'ṇ'], ['"s', 'ś'], ['.s', 'ṣ'], ['~m', 'm̐']];
    return rep(s, T);
  }
  function fromSLP1(s) {
    const T = [['A', 'ā'], ['I', 'ī'], ['U', 'ū'], ['f', 'ṛ'], ['F', 'ṝ'], ['x', 'ḷ'], ['X', 'ḹ'], ['E', 'ai'], ['O', 'au'],
      ['M', 'ṃ'], ['H', 'ḥ'], ['K', 'kh'], ['G', 'gh'], ['N', 'ṅ'], ['C', 'ch'], ['J', 'jh'], ['Y', 'ñ'], ['w', 'ṭ'], ['W', 'ṭh'],
      ['q', 'ḍ'], ['Q', 'ḍh'], ['R', 'ṇ'], ['T', 'th'], ['D', 'dh'], ['P', 'ph'], ['B', 'bh'], ['S', 'ś'], ['z', 'ṣ'], ['~', 'm̐']];
    return rep(s, T);
  }
  function fromITRANS(s) {
    const T = [['RRi', 'ṛ'], ['RRI', 'ṝ'], ['LLi', 'ḷ'], ['LLI', 'ḹ'], ['R^i', 'ṛ'], ['R^I', 'ṝ'], ['L^i', 'ḷ'], ['L^I', 'ḹ'],
      ['aa', 'ā'], ['A', 'ā'], ['ii', 'ī'], ['I', 'ī'], ['uu', 'ū'], ['U', 'ū'], ['.N', 'm̐'], ['M', 'ṃ'], ['.n', 'ṃ'], ['H', 'ḥ'],
      ['~N', 'ṅ'], ['N^', 'ṅ'], ['~n', 'ñ'], ['JN', 'ñ'], ['chh', 'ch'], ['Ch', 'ch'], ['ch', 'c'], ['Th', 'ṭh'], ['T', 'ṭ'],
      ['Dh', 'ḍh'], ['D', 'ḍ'], ['N', 'ṇ'], ['sh', 'ś'], ['Sh', 'ṣ'], ['shh', 'ṣ'], ['.a', "'"], ['w', 'v']];
    return rep(s, T);
  }
  function rep(s, T) {
    // longest-match, single left-to-right pass; keeps regex escapes (\x) untouched
    T = T.slice().sort((a, b) => b[0].length - a[0].length);
    let out = '';
    for (let i = 0; i < s.length;) {
      if (s[i] === '\\' && i + 1 < s.length) { out += s[i] + s[i + 1]; i += 2; continue; }
      let hit = null;
      for (const [a, b] of T) if (s.startsWith(a, i)) { hit = [a, b]; break; }
      if (hit) { out += hit[1]; i += hit[0].length; } else { out += s[i]; i++; }
    }
    return out;
  }
  function fromScheme(s, scheme) {
    switch (scheme) {
      case 'hk': return fromHK(s);
      case 'velthuis': return fromVelthuis(s);
      case 'slp1': return fromSLP1(s);
      case 'itrans': return fromITRANS(s);
      default: return s;
    }
  }

  // ---------------- display transliteration ----------------
  // Devanagari → IAST, preserving punctuation and case of Latin text.
  function devaToIAST(s) {
    if (!SLOW_RE.test(s)) return s;
    let out = '';
    for (let i = 0; i < s.length; i++) {
      const c = s[i], code = s.charCodeAt(i);
      if (!isDevaCode(code)) { out += c; continue; }
      let base = CONS[c];
      if (base !== undefined) {
        let j = i + 1;
        if (s[j] === NUKTA_SIGN) { base = NUKTA[c] || base; j++; }
        out += base;
        const nx = s[j];
        if (nx === VIRAMA) { i = j; continue; }
        const m = nx !== undefined ? MATRA[nx] : undefined;
        if (m !== undefined) { out += m; i = j; continue; }
        out += 'a'; i = j - 1; continue;
      }
      if (c === '।') { out += '|'; continue; }
      if (c === '॥') { out += '||'; continue; }
      const t = OTHER[c];
      if (t !== undefined) out += t;
      else if (MATRA[c] !== undefined) out += MATRA[c];
    }
    return out;
  }

  // IAST → Devanagari (for display only).
  const V_IND = { 'a': 'अ', 'ā': 'आ', 'i': 'इ', 'ī': 'ई', 'u': 'उ', 'ū': 'ऊ', 'ṛ': 'ऋ', 'ṝ': 'ॠ', 'ḷ': 'ऌ', 'ḹ': 'ॡ', 'e': 'ए', 'ai': 'ऐ', 'o': 'ओ', 'au': 'औ' };
  const V_MAT = { 'a': '', 'ā': 'ा', 'i': 'ि', 'ī': 'ी', 'u': 'ु', 'ū': 'ू', 'ṛ': 'ृ', 'ṝ': 'ॄ', 'ḷ': 'ॢ', 'ḹ': 'ॣ', 'e': 'े', 'ai': 'ै', 'o': 'ो', 'au': 'ौ' };
  const C_DEV = {
    'kh': 'ख', 'gh': 'घ', 'ch': 'छ', 'jh': 'झ', 'ṭh': 'ठ', 'ḍh': 'ढ', 'th': 'थ', 'dh': 'ध', 'ph': 'फ', 'bh': 'भ',
    'k': 'क', 'g': 'ग', 'ṅ': 'ङ', 'c': 'च', 'j': 'ज', 'ñ': 'ञ', 'ṭ': 'ट', 'ḍ': 'ड', 'ṇ': 'ण', 't': 'त', 'd': 'द', 'n': 'न',
    'p': 'प', 'b': 'ब', 'm': 'म', 'y': 'य', 'r': 'र', 'l': 'ल', 'v': 'व', 'ś': 'श', 'ṣ': 'ष', 's': 'स', 'h': 'ह', 'ḻ': 'ळ'
  };
  const MISC_DEV = { 'ṃ': 'ं', 'ṁ': 'ं', 'ḥ': 'ः', "'": 'ऽ', '’': 'ऽ', '|': '।', '0': '०', '1': '१', '2': '२', '3': '३', '4': '४', '5': '५', '6': '६', '7': '७', '8': '८', '9': '९' };
  function iastToDeva(s) {
    s = s.normalize('NFC').replace(/r̥̄/g, 'ṝ').replace(/r̥/g, 'ṛ').replace(/l̥̄/g, 'ḹ').replace(/l̥/g, 'ḷ').replace(/ṁ/g, 'ṃ').replace(/ē/g, 'e').replace(/ō/g, 'o').replace(/m̐/g, '\u0001');
    let out = '';
    const lower = s.toLowerCase();
    let i = 0;
    const two = (k) => lower.substr(k, 2);
    while (i < s.length) {
      if (lower[i] === '\u0001') { out += 'ँ'; i++; continue; }
      if (lower.substr(i, 2) === '||') { out += '॥'; i += 2; continue; }
      if (lower.substr(i, 2) === '//') { out += '॥'; i += 2; continue; }
      if (lower[i] === '/') { out += '।'; i++; continue; }
      let cons = null;
      if (C_DEV[two(i)] && two(i).length === 2) cons = two(i);
      else if (C_DEV[lower[i]]) cons = lower[i];
      if (cons) {
        out += C_DEV[cons]; i += cons.length;
        let v = null;
        if (V_MAT[two(i)] !== undefined && (two(i) === 'ai' || two(i) === 'au')) v = two(i);
        else if (V_MAT[lower[i]] !== undefined) v = lower[i];
        if (v) { out += V_MAT[v]; i += v.length; }
        else out += '्';
        continue;
      }
      let v = null;
      if (two(i) === 'ai' || two(i) === 'au') v = two(i);
      else if (V_IND[lower[i]]) v = lower[i];
      if (v) { out += V_IND[v]; i += v.length; continue; }
      const m = MISC_DEV[lower[i]];
      out += m !== undefined ? m : s[i];
      i++;
    }
    return out.replace(/्(\s|$|[।॥,;.:!?\-])/g, '्$1');
  }

  function hasDeva(s) { return /[\u0900-\u097F]/.test(s); }

  const API = { normalize, fastKey, fromScheme, devaToIAST, iastToDeva, hasDeva, stripChar };
  G.SaritTranslit = API;
  if (typeof module !== 'undefined' && module.exports) module.exports = API;
})(typeof self !== 'undefined' ? self : globalThis);
