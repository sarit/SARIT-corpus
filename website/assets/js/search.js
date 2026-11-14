/* SARIT search — search page controller. */
(function () {
  'use strict';
  const T = self.SaritTranslit, C = self.SaritCommon, S = C.S;
  const $ = (s, r) => (r || document).querySelector(s);
  const $$ = (s, r) => Array.from((r || document).querySelectorAll(s));
  const form = $('#search-form');

  // ---------------- IAST palette ----------------
  const PAL = ['ā', 'ī', 'ū', 'ṛ', 'ṝ', 'ḷ', 'ṃ', 'ḥ', 'ṅ', 'ñ', 'ṭ', 'ḍ', 'ṇ', 'ś', 'ṣ', "'"];
  $('#palette').innerHTML = PAL.map(c => '<button type="button" class="pal" tabindex="-1">' + C.esc(c) + '</button>').join('');
  $('#palette').addEventListener('mousedown', e => e.preventDefault());
  $('#palette').addEventListener('click', e => {
    const b = e.target.closest('.pal'); if (!b) return;
    const q = $('#q'), c = b.textContent;
    const a = q.selectionStart ?? q.value.length, z = q.selectionEnd ?? q.value.length;
    q.value = q.value.slice(0, a) + c + q.value.slice(z);
    q.focus(); q.setSelectionRange(a + c.length, a + c.length);
  });

  // ---------------- state <-> form <-> URL ----------------
  const FIELDS = ['q', 'mode', 'nd', 'ns', 'na', 'ww', 'notes', 'scheme', 'script', 'max', 'ctx', 'sort', 'coll'];
  const DEFAULTS = { mode: 'phrase', nd: false, ns: false, na: false, ww: false, notes: true, scheme: 'auto', script: 'orig', max: '200', ctx: '200', sort: 'cat', coll: 'all' };
  let selected = new Set();

  function readForm() {
    const f = form.elements;
    return {
      q: f.q.value, mode: f.mode.value, nd: f.nd.checked, ns: f.ns.checked, na: f.na.checked, ww: f.ww.checked,
      notes: f.notes.checked, scheme: f.scheme.value, script: f.script.value, max: f.max.value, ctx: f.ctx.value, sort: f.sort.value, coll: f.coll.value
    };
  }
  function writeForm(st) {
    const f = form.elements;
    f.q.value = st.q || '';
    for (const k of ['nd', 'ns', 'na', 'ww', 'notes']) f[k].checked = !!st[k];
    for (const k of ['mode', 'scheme', 'script', 'max', 'ctx', 'sort', 'coll']) {
      const el = f[k];
      if (el instanceof RadioNodeList) { for (const r of el) r.checked = (r.value === st[k]); }
      else el.value = st[k];
    }
  }
  function stateFromURL() {
    const p = new URLSearchParams(location.search);
    const st = Object.assign({}, DEFAULTS);
    if (p.has('q')) st.q = p.get('q');
    for (const k of ['mode', 'scheme', 'script', 'max', 'ctx', 'sort', 'coll']) if (p.has(k)) st[k] = p.get(k);
    for (const k of ['nd', 'ns', 'na', 'ww', 'notes']) if (p.has(k)) st[k] = p.get(k) === '1';
    if (p.has('texts')) { selected = new Set(p.get('texts').split(',').filter(Boolean)); if (!p.has('coll')) st.coll = 'sel'; }
    return st;
  }
  function urlFromState(st) {
    const p = new URLSearchParams();
    p.set('q', st.q);
    for (const k of FIELDS) {
      if (k === 'q') continue;
      const v = st[k], d = DEFAULTS[k];
      if (typeof d === 'boolean') { if (!!v !== d) p.set(k, v ? '1' : '0'); }
      else if (v !== d) p.set(k, v);
    }
    if (st.coll === 'sel' && selected.size) p.set('texts', Array.from(selected).join(','));
    return location.pathname + '?' + p.toString();
  }

  // ---------------- text picker ----------------
  let CAT = null;
  function updateSelCount() { $('#sel-count').textContent = selected.size ? '(' + selected.size + ')' : ''; }
  function renderPicker() {
    const f = $('#pick-filter').value.trim().toLowerCase();
    const fk = f ? T.fastKey(f, { noDiac: true }) : '';
    const rows = CAT.list.filter(t => !fk || T.fastKey(t.title + ' ' + C.authorsOf(t) + ' ' + t.file, { noDiac: true }).includes(fk));
    $('#pick-list').innerHTML = rows.slice(0, 1200).map(t =>
      '<label class="pick"><input type="checkbox" value="' + C.esc(t.id) + '"' + (selected.has(t.id) ? ' checked' : '') + '> ' +
      '<span class="pick-title">' + C.esc(t.title) + '</span>' +
      (C.authorsOf(t) ? ' <span class="muted">' + C.esc(C.authorsOf(t)) + '</span>' : '') +
      ' <span class="badge badge-' + t.coll.toLowerCase() + '">' + t.coll + '</span></label>').join('') +
      (rows.length > 1200 ? '<p class="muted">… ' + (rows.length - 1200) + ' more; narrow the filter.</p>' : '');
  }
  $('#pick-filter').addEventListener('input', () => CAT && renderPicker());
  $('#pick-list').addEventListener('change', e => {
    const cb = e.target; if (cb.type !== 'checkbox') return;
    if (cb.checked) selected.add(cb.value); else selected.delete(cb.value);
    updateSelCount();
  });
  $('#pick-all').addEventListener('click', () => { $$('#pick-list input').forEach(cb => { cb.checked = true; selected.add(cb.value); }); updateSelCount(); });
  $('#pick-none').addEventListener('click', () => { selected.clear(); $$('#pick-list input').forEach(cb => cb.checked = false); updateSelCount(); });
  function syncPicker() { $('#picker').hidden = form.elements.coll.value !== 'sel'; }
  $$('input[name=coll]').forEach(r => r.addEventListener('change', syncPicker));
  // whole-word makes no sense with word division ignored
  function syncOpts() {
    const ns = form.elements.ns.checked;
    form.elements.ww.disabled = ns;
    form.elements.ww.closest('label').classList.toggle('disabled', ns);
  }
  form.elements.ns.addEventListener('change', syncOpts);

  // ---------------- query compilation ----------------
  const reEsc = s => s.replace(/[\\^$.*+?()[\]{}|\/-]/g, '\\$&');
  function wrapWhole(re) { return '(?<![\\p{L}\\p{M}\\p{N}])(?:' + re + ')(?![\\p{L}\\p{M}\\p{N}])'; }

  // Transliterate/normalise the literal parts of a regular expression.
  function normRegex(pat, opts) {
    let out = '', run = '', inClass = false;
    const flush = () => {
      if (!run) return;
      let k = T.fastKey(run, opts);
      out += inClass ? k.replace(/[\\\]\[^-]/g, '\\$&') : reEsc(k);
      run = '';
    };
    for (let i = 0; i < pat.length; i++) {
      const c = pat[i];
      if (c === '\\') {
        flush();
        const nx = pat[i + 1] || '';
        if ((nx === 'p' || nx === 'P' || nx === 'u') && pat[i + 2] === '{') {
          const j = pat.indexOf('}', i + 2);
          const end = j < 0 ? pat.length : j + 1;
          out += pat.slice(i, end); i = end - 1; continue;
        }
        if (nx === 'u' || nx === 'x') { const len = nx === 'u' ? 6 : 4; out += pat.slice(i, i + len); i += len - 1; continue; }
        // escaped literal letter that is not a class escape (e.g. \|) passes through
        out += c + nx; i++; continue;
      }
      if (inClass) {
        if (c === ']') { flush(); out += c; inClass = false; continue; }
        if (c === '-' || (c === '^' && out.endsWith('['))) { flush(); out += c; continue; }
        run += c; continue;
      }
      if (c === '[') { flush(); out += c; inClass = true; continue; }
      if ('^$.|?*+(){}'.indexOf(c) >= 0) {
        flush();
        out += c;
        // keep quantifier braces and group syntax as typed: (?: (?= (?! (?<= (?<!  {m,n}
        if (c === '(' && pat[i + 1] === '?') {
          const m = pat.slice(i + 1).match(/^\?(?:[:=!]|<[=!]|<[A-Za-z_]\w*>)/);
          if (m) { out += m[0]; i += m[0].length; }
        }
        if (c === '{') { const m = pat.slice(i + 1).match(/^\d*,?\d*\}/); if (m) { out += m[0]; i += m[0].length; } }
        continue;
      }
      run += c;
    }
    flush();
    return out;
  }

  // Boolean operators: the words AND / OR (in capitals) between search terms.
  // Returns null when there are none, else {op, groups} where each group is a term (may be a phrase).
  function splitBool(src, sep) {
    const parts = src.split(sep === 'regex' ? /\s+(AND|OR)\s+/ : /\s+(AND|OR)\s+|^(?:AND|OR)\s+|\s+(?:AND|OR)$/);
    if (parts.length === 1) return null;
    const ops = new Set(parts.filter((p, i) => i % 2 === 1 && p));
    if (ops.size > 1) throw new Error('Use either AND or OR in one search, not both.');
    const groups = parts.filter((p, i) => i % 2 === 0).map(s => (s || '').trim()).filter(Boolean);
    if (!groups.length) throw new Error('AND / OR need search terms on both sides.');
    return { op: ops.size ? ops.values().next().value : 'AND', groups };
  }

  // A search term with wildcards: * = any letters within a word, ? = exactly one letter.
  // Returns {re, lit}: a regex source, and the longest literal piece (used to find candidates fast).
  const WCH = "[\\p{L}\\p{M}\\p{N}']";
  function termPattern(term, opts) {
    let parts = term.split(/([*?])/).filter(p => p !== '');
    if (opts.noSep) { // words run together: leading/trailing * add nothing and would span the passage
      while (parts[0] === '*') parts.shift();
      while (parts[parts.length - 1] === '*') parts.pop();
    }
    let re = '', lit = '', wild = false, show = '';
    parts.forEach((p, i) => {
      if (p === '*' || p === '?') show += p;
      if (p === '*') {
        wild = true;
        if (i === 0) re += '(?<!' + WCH + ')' + WCH + '*?';   // from the start of the word
        else if (i === parts.length - 1) re += WCH + '*';      // to the end of the word
        else re += WCH + '*?';
      } else if (p === '?') { wild = true; re += WCH; }
      else { const k = T.fastKey(p, opts); re += reEsc(k); show += k; if (k.length > lit.length) lit = k; }
    });
    return { re, lit, wild, show, key: wild ? null : T.fastKey(parts.join(''), opts) };
  }

  // Leading and trailing .* do not change which passages match, but on very long
  // passages they make the regex engine backtrack until it runs out of stack.
  const trimDotStar = s => s.replace(/^(?:\.\*)+/, '').replace(/(?:^|[^\\])(?:\.\*)+$/, m => m.replace(/(?:\.\*)+$/, ''));

  function compile(st) {
    const opts = { noDiac: st.nd, noSep: st.ns, nasal: st.na };
    const conv = s => st.scheme !== 'auto' ? T.fromScheme(s, st.scheme) : s;
    const src = st.q.trim();
    const whole = st.ww && !st.ns;
    const q = { mode: st.mode, whole, kinds: st.notes ? '.hnv' : '.h' };
    const test = re => { new RegExp(re, 'gmu'); return re; }; // throws on syntax errors
    if (st.mode === 'regex') {
      const b = splitBool(src, 'regex');
      const res = (b ? b.groups : [src]).map(g => {
        let r = normRegex(trimDotStar(conv(g)), opts);
        if (!r) throw new Error('“' + g + '” matches everything; give some letters to look for.');
        return test(whole ? wrapWhole(r) : r);
      });
      if (b && b.op === 'AND') { q.mode = 'all'; q.termRes = res; q.terms = res.map(() => ''); q.display = res.join(' AND '); }
      else { q.regex = res.length > 1 ? res.map(r => '(?:' + r + ')').join('|') : res[0]; q.display = q.regex; }
    } else {
      const b = splitBool(src);
      let mode = st.mode, raw;
      if (b) { mode = b.op === 'AND' ? 'all' : 'any'; raw = b.groups; }
      else raw = st.mode === 'phrase' ? [src.split(/\s+/).join(' ')] : src.split(/\s+/).filter(Boolean);
      let terms = raw.map(r => termPattern(conv(r), opts)).filter(t => t.re);
      const seen = new Set(); terms = terms.filter(t => !seen.has(t.re) && seen.add(t.re));
      if (terms.some(t => !t.lit)) throw new Error('Each search term needs at least one letter besides * and ?.');
      const anyWild = terms.some(t => t.wild);
      const disp = terms.map(t => t.show);
      q.mode = mode;
      if (mode === 'phrase') {
        const t = terms[0];
        q.terms = [t.key || t.lit];
        if (whole || anyWild) { q.mode = 'regex'; q.regex = test(whole ? wrapWhole(t.re) : t.re); }
        q.display = disp[0];
      } else if (mode === 'any') {
        const alt = terms.map(t => t.re).join('|');
        q.terms = terms.map(t => t.lit);
        q.regex = test(whole ? wrapWhole(alt) : alt);
        q.display = disp.join(' OR ');
      } else { // all: the term with the longest literal part is used to find candidate passages
        const order = terms.map((t, i) => i).sort((a, c) => terms[c].lit.length - terms[a].lit.length);
        terms = order.map(i => terms[i]);
        q.terms = terms.map(t => t.lit);
        if (whole || anyWild) q.termRes = terms.map(t => test(whole ? wrapWhole(t.re) : t.re));
        q.display = order.map(i => disp[i]).join(' AND ');
      }
    }
    const sig = (opts.noDiac ? 'd' : '') + (opts.noSep ? 's' : '') + (opts.nasal ? 'n' : '');
    return { q, opts, sig };
  }

  // ---------------- worker pool ----------------
  const NW = Math.max(1, Math.min(4, (navigator.hardwareConcurrency || 2) - 1));
  // total characters of search keys kept in memory (≈2 bytes each)
  const MEM = (navigator.deviceMemory || 4) >= 8 ? 260e6 : 120e6;
  const BUDGET = Math.floor(MEM / NW);
  const workers = [];
  function pool() {
    if (workers.length) return workers;
    for (let i = 0; i < NW; i++) {
      const w = new Worker(S.base + 'assets/js/search-worker.js');
      w.postMessage({ type: 'init', base: S.base, budget: BUDGET });
      w.onmessage = onWorker;
      w.onerror = e => { console.error(e); status('A search worker failed: ' + (e.message || e), true); };
      workers.push(w);
    }
    return workers;
  }

  // ---------------- search run ----------------
  let run = null, sidSeq = 0;

  function status(html, err) { const el = $('#status'); el.innerHTML = html; el.classList.toggle('error', !!err); }

  function textsInScope(st) {
    if (st.coll === 'sel') return CAT.list.filter(t => selected.has(t.id));
    if (st.coll === 'SARIT' || st.coll === 'GRETIL') return CAT.list.filter(t => t.coll === st.coll);
    return CAT.list.slice();
  }

  function startSearch(pushHistory) {
    const st = readForm();
    if (!st.q.trim()) { status('Type something to search for.'); return; }
    if (!CAT) { status('The list of texts is still loading; try again in a moment.'); return; }
    let comp;
    try { comp = compile(st); } catch (e) { status((e instanceof SyntaxError ? 'Your regular expression is not valid: ' : '') + C.esc(e.message), true); return; }
    if ((comp.q.terms && !comp.q.termRes && !comp.q.regex && !comp.q.terms.join('')) || (comp.q.mode === 'regex' && !comp.q.regex)) { status('Nothing left to search for after normalisation.'); return; }
    const texts = textsInScope(st);
    if (!texts.length) { status('No texts selected. Tick some texts in the list, or choose “all texts”.'); return; }
    if (pushHistory) history.pushState(null, '', urlFromState(st));
    cancel();
    const sid = ++sidSeq;
    run = {
      sid, st, comp, texts, started: performance.now(), results: new Map(), doneWorkers: 0, textsDone: 0,
      totalChars: texts.reduce((a, t) => a + t.chars, 0), charsDone: 0, collapsed: new Set()
    };
    // balance by size across workers (largest first)
    const ws = pool();
    const queues = ws.map(() => ({ ids: [], chars: 0 }));
    texts.slice().sort((a, b) => b.chars - a.chars).forEach(t => {
      let best = queues[0]; for (const qq of queues) if (qq.chars < best.chars) best = qq;
      best.ids.push(t.id); best.chars += t.chars;
    });
    ws.forEach((w, i) => w.postMessage({
      type: 'search', sid, texts: queues[i].ids, q: comp.q, opts: comp.opts, sig: comp.sig,
      maxSegsPerText: parseInt(st.max, 10), ctxChars: parseInt(st.ctx, 10)
    }));
    $('#stop').hidden = false; $('#progress').hidden = false; $('#toolbar').hidden = true;
    $('#results').innerHTML = '';
    status('Searching ' + C.fmtNum(texts.length) + ' text' + (texts.length > 1 ? 's' : '') + ' for <code>' + C.esc(comp.q.display) + '</code>…' +
      (texts.length > 50 ? ' <span class="muted">(The first search over many texts downloads them; later searches are faster.)</span>' : ''));
    progress();
  }

  function cancel() {
    if (run && run.doneWorkers < workers.length) workers.forEach(w => w.postMessage({ type: 'cancel', sid: run.sid }));
    $('#stop').hidden = true;
  }

  function progress() {
    if (!run) return;
    const f = run.totalChars ? run.charsDone / run.totalChars : 1;
    $('#progress .bar').style.width = (100 * f).toFixed(1) + '%';
  }

  let renderTimer = null;
  function scheduleRender() {
    if (renderTimer) return;
    renderTimer = setTimeout(() => { renderTimer = null; render(); }, 250);
  }

  function onWorker(ev) {
    const d = ev.data;
    if (!run || d.sid !== run.sid) return;
    if (d.type === 'result') {
      run.textsDone++;
      const t = CAT.byId.get(d.text);
      run.charsDone += t ? t.chars : 0;
      if (d.total || d.error) run.results.set(d.text, d);
      progress(); scheduleRender();
    } else if (d.type === 'done') {
      run.doneWorkers++;
      if (run.doneWorkers === workers.length) { finish(); }
    }
  }

  function finish() {
    $('#stop').hidden = true;
    $('#progress').hidden = true;
    run.finished = true;
    render();
  }

  // ---------------- rendering ----------------
  function summary() {
    let hits = 0, segs = 0, texts = 0, errs = 0;
    for (const r of run.results.values()) { if (r.error) { errs++; continue; } hits += r.total; segs += r.segsHit; texts++; }
    const secs = ((performance.now() - run.started) / 1000).toFixed(1);
    let s = '<strong>' + C.fmtNum(hits) + '</strong> match' + (hits === 1 ? '' : 'es') + ' in ' + C.fmtNum(segs) + ' passage' + (segs === 1 ? '' : 's') +
      ' of ' + C.fmtNum(texts) + ' text' + (texts === 1 ? '' : 's');
    s += run.finished ? ' <span class="muted">(' + C.fmtNum(run.textsDone) + ' texts searched in ' + secs + ' s; key: <code>' + C.esc(run.comp.q.display) + '</code>)</span>'
      : ' <span class="muted">so far — ' + C.fmtNum(run.textsDone) + ' of ' + C.fmtNum(run.texts.length) + ' texts searched</span>';
    if (errs) s += ' <span class="error">' + errs + ' text(s) could not be searched; see the end of the list.</span>';
    return s;
  }

  function hitHTML(t, h, script) {
    const cls = C.scriptClass(h.snippet, script);
    const ref = h.ref ? '<span class="ref">' + C.esc(h.ref) + '</span>' : '';
    const ctx = h.ctx ? '<span class="ctx" title="Position in the text’s division hierarchy">' + C.esc(h.ctx) + '</span>' : '';
    const loc = h.loc ? '<span class="loc" title="Page.line of the printed edition (from &lt;pb/&gt; and &lt;lb/&gt; in the XML)">' + C.esc(C.locLabel(h.loc)) + '</span>' : '';
    const kind = h.kind === 'n' ? '<span class="kind">note</span>' : h.kind === 'v' ? '<span class="kind">variant</span>' : h.kind === 'h' ? '<span class="kind">heading</span>' : '';
    return '<article class="hit" data-t="' + C.esc(t.id) + '" data-s="' + h.seg + '">' +
      '<div class="cite">' + ref + kind + ctx + loc +
      '<span class="acts">' +
      '<a href="' + C.esc(C.readerUrl(t.id, h.seg)) + '" title="Read this passage in context">context</a>' +
      '<a href="' + C.esc(C.xmlUrl(t, h.line)) + '" title="Open the TEI source at this line on GitHub" target="_blank" rel="noopener">XML l.' + h.line + '</a>' +
      '<button type="button" class="linkish copy-cite" title="Copy a full citation">cite</button>' +
      '</span></div>' +
      '<div class="snip ' + cls + '">' + (h.pre ? '… ' : '') + C.renderText(h.snippet, h.hls, script) + (h.post ? ' …' : '') + '</div>' +
      '</article>';
  }

  // Texts are listed in catalogue order (SARIT, then GRETIL, each by title) or by number of
  // matching passages; passages within a text are always in the order of the text.
  function sortedGroups() {
    const g = Array.from(run.results.values()).filter(r => !r.error).map(r => ({ r, t: CAT.byId.get(r.text) }));
    if (run.st.sort === 'hits') return g.sort((a, b) => b.r.segsHit - a.r.segsHit || b.r.total - a.r.total || a.t.order - b.t.order);
    return g.sort((a, b) => a.t.order - b.t.order);
  }

  function render() {
    if (!run) return;
    status(summary());
    const script = run.st.script;
    const groups = sortedGroups();
    const errs = Array.from(run.results.values()).filter(r => r.error);
    const html = groups.map(({ r, t }) => {
      const open = !run.collapsed.has(t.id);
      return '<section class="tgroup" data-t="' + C.esc(t.id) + '">' +
        '<details' + (open ? ' open' : '') + '><summary><span class="tg-title">' + C.esc(t.title) + '</span>' +
        (C.authorsOf(t) ? ' <span class="tg-author">' + C.esc(C.authorsOf(t)) + '</span>' : '') +
        ' <span class="badge badge-' + t.coll.toLowerCase() + '">' + t.coll + '</span>' +
        ' <span class="tg-count">' + C.fmtNum(r.total) + ' match' + (r.total === 1 ? '' : 'es') + ' in ' + C.fmtNum(r.segsHit) + ' passage' + (r.segsHit === 1 ? '' : 's') + '</span>' +
        '</summary>' +
        '<div class="tg-meta muted">' + C.esc(t.file) + (t.source && t.source[0] ? ' · ' + C.esc(t.source[0].slice(0, 180)) : '') + '</div>' +
        r.hits.map(h => hitHTML(t, h, script)).join('') +
        (r.truncated ? '<p class="more">Showing ' + r.hits.length + ' of ' + C.fmtNum(r.segsHit) + ' passages. <a href="' + C.esc(onlyThisText(t.id)) + '">Show all in this text</a></p>' : '') +
        '</details></section>';
    }).join('') + errs.map(r => '<p class="error">' + C.esc(r.text) + ': ' + C.esc(r.error) + '</p>').join('');
    $('#results').innerHTML = html || (run.finished ? '<p class="nohits">No matches. Try “ignore diacritics”, “ignore word division”, or a shorter search term.</p>' : '');
    $('#toolbar').hidden = !groups.length;
  }

  function onlyThisText(id) {
    const st = Object.assign({}, run.st, { coll: 'sel', max: '100000' });
    const keep = selected; selected = new Set([id]);
    const u = urlFromState(st); selected = keep; return u;
  }

  $('#results').addEventListener('toggle', e => {
    const sec = e.target.closest && e.target.closest('.tgroup'); if (!sec || !run) return;
    if (e.target.open) run.collapsed.delete(sec.dataset.t); else run.collapsed.add(sec.dataset.t);
  }, true);

  function findHit(el) {
    const art = el.closest('.hit'); if (!art) return null;
    const r = run.results.get(art.dataset.t); if (!r) return null;
    const h = r.hits.find(x => String(x.seg) === art.dataset.s);
    return { t: CAT.byId.get(art.dataset.t), h };
  }
  $('#results').addEventListener('click', e => {
    const b = e.target.closest('.copy-cite'); if (!b) return;
    const x = findHit(b); if (x) C.copyText(C.citation(x.t, x.h), b);
  });

  function allHits() {
    const out = [];
    const groups = sortedGroups();
    for (const { r, t } of groups) for (const h of r.hits) out.push({ t, h });
    return out;
  }
  $('#copy-all').addEventListener('click', e => {
    C.copyText(allHits().map(({ t, h }) => C.citation(t, h) + '\n    ' + h.snippet.replace(/\u2028/g, ' / ')).join('\n\n'), e.target);
  });
  $('#dl-tsv').addEventListener('click', () => {
    const cell = s => String(s == null ? '' : s).replace(/[\t\n\r\u2028]/g, ' ');
    const rows = [['title', 'author', 'collection', 'file', 'ref', 'division', 'edition_loc', 'kind', 'xml_line', 'xml_url', 'matches', 'text']];
    for (const { t, h } of allHits()) rows.push([t.title, C.authorsOf(t), t.coll, t.file, h.ref, h.ctx, h.loc, h.kind, h.line, C.xmlUrl(t, h.line), h.n, (h.pre ? '… ' : '') + h.snippet + (h.post ? ' …' : '')]);
    const blob = new Blob(['﻿' + rows.map(r => r.map(cell).join('\t')).join('\n')], { type: 'text/tab-separated-values;charset=utf-8' });
    const a = document.createElement('a');
    a.href = URL.createObjectURL(blob);
    a.download = 'sarit-search-' + run.st.q.trim().slice(0, 30).replace(/[^\p{L}\p{N}]+/gu, '_') + '.tsv';
    document.body.appendChild(a); a.click(); a.remove();
    setTimeout(() => URL.revokeObjectURL(a.href), 5000);
  });
  $('#collapse-all').addEventListener('click', e => {
    const dets = $$('#results details');
    const anyOpen = dets.some(d => d.open);
    dets.forEach(d => d.open = !anyOpen);
    e.target.textContent = anyOpen ? 'Expand all' : 'Collapse all';
  });
  $('#copy-link').addEventListener('click', e => C.copyText(location.origin + urlFromState(run ? run.st : readForm()), e.target));

  // re-render (without searching again) when the display script changes
  form.elements.sort.addEventListener('change', () => { if (run) { run.st.sort = form.elements.sort.value; history.replaceState(null, '', urlFromState(run.st)); render(); } });
  form.elements.script.addEventListener('change', () => { if (run) { run.st.script = form.elements.script.value; history.replaceState(null, '', urlFromState(run.st)); render(); } });

  form.addEventListener('submit', e => { e.preventDefault(); startSearch(true); });
  $('#stop').addEventListener('click', () => { cancel(); if (run) { run.finished = true; $('#progress').hidden = true; render(); status(summary() + ' <span class="muted">— stopped.</span>'); } });
  window.addEventListener('popstate', () => { writeForm(stateFromURL()); syncPicker(); syncOpts(); if (form.elements.q.value.trim()) startSearch(false); });

  // ---------------- boot ----------------
  const st0 = stateFromURL();
  writeForm(st0); syncPicker(); syncOpts(); updateSelCount();
  C.catalogue().then(cat => {
    CAT = cat;
    renderPicker();
    const n = cat.list.length, chars = cat.list.reduce((a, t) => a + t.chars, 0);
    if (st0.q && st0.q.trim()) startSearch(false);
    else status(C.fmtNum(n) + ' texts, ' + C.fmtSize(chars) + '. Type in IAST, Devanagari or plain ASCII; see <a href="' + S.base + 'help.html">Help</a> for options.');
  }).catch(e => status(C.esc(e.message), true));
})();
