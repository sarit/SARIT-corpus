/* SARIT search — reader: shows a text passage in context with its references. */
(function () {
  'use strict';
  const T = self.SaritTranslit, C = self.SaritCommon, S = C.S;
  const $ = s => document.querySelector(s);
  const p = new URLSearchParams(location.search);
  const id = p.get('t');
  let target = parseInt(p.get('s') || '0', 10) || 0;
  let lines = null, kinds = '', meta = null, t = null;
  let from = 0, to = 0;
  const CHUNK = 150;

  function status(s, err) { const el = $('#r-status'); el.innerHTML = s || ''; el.classList.toggle('error', !!err); }

  function header() {
    const au = C.authorsOf(t);
    let h = '<h1 class="r-title">' + C.esc(t.title) + '</h1>';
    if (t.subtitle) h += '<p class="r-sub">' + C.esc(t.subtitle) + '</p>';
    h += '<dl class="r-meta">';
    if (au) h += '<dt>Author</dt><dd>' + C.esc(au) + '</dd>';
    if (t.source) h += '<dt>Source</dt><dd>' + t.source.map(C.esc).join('<br>') + '</dd>';
    if (t.gretilHeader) h += '<dt>GRETIL header</dt><dd>' + t.gretilHeader.map(C.esc).join('<br>') + '</dd>';
    if (t.resp) h += '<dt>Responsibility</dt><dd>' + t.resp.map(C.esc).join('<br>') + '</dd>';
    h += '<dt>File</dt><dd><a href="' + C.esc(C.xmlUrl(t)) + '">' + C.esc(t.file) + '</a> · ' + t.coll + ' · ' + C.fmtNum(t.segs) + ' passages, ' + C.fmtSize(t.chars) + '</dd>';
    if (t.gretilUrl) h += '<dt>GRETIL</dt><dd><a href="' + C.esc(t.gretilUrl) + '">' + C.esc(t.gretilUrl) + '</a></dd>';
    if (t.licence) h += '<dt>Licence</dt><dd>' + (/^https?:/.test(t.licence) ? '<a href="' + C.esc(t.licence) + '">' + C.esc(t.licence) + '</a>' : C.esc(t.licence)) + '</dd>';
    h += '</dl>';
    $('#r-head').innerHTML = h;
    document.title = t.title + ' · SARIT Corpus Search';
    $('#r-search').href = S.base + '?coll=sel&texts=' + encodeURIComponent(t.id) + '&q=';
  }

  function segHTML(i, script, showPages) {
    const m = meta.segs[i];
    const text = lines[i];
    const kind = kinds[i];
    const marks = (m[4] || []).filter(x => x[1] === 'r' || showPages);
    // insert markers into text
    let html = '', pos = 0;
    const conv = s => script === 'iast' ? T.devaToIAST(s) : script === 'deva' ? (T.hasDeva(s) ? s : T.iastToDeva(s)) : s;
    const piece = s => C.esc(conv(s)).replace(/\u2028/g, '\n');
    for (const mk of marks) {
      if (mk[0] > pos) { html += piece(text.slice(pos, mk[0])); pos = mk[0]; }
      html += mk[1] === 'r' ? '<span class="mk-ref">' + C.esc(mk[2]) + '</span>' : '<span class="mk-pg">' + C.esc(mk[2]) + '</span>';
    }
    html += piece(text.slice(pos));
    const firstRef = m[1] || ((m[4] || []).find(x => x[1] === 'r') || [])[2] || '';
    const cls = 'seg k-' + (kind === '.' ? 't' : kind) + (i === target ? ' target' : '') + ' ' + C.scriptClass(text, script);
    return '<div class="' + cls + '" id="s' + i + '" data-i="' + i + '">' +
      '<div class="gutter">' + (firstRef ? '<span class="g-ref">' + C.esc(firstRef) + '</span>' : '') +
      (m[2] ? '<span class="g-loc">' + C.esc(C.locLabel(m[2])) + '</span>' : '') +
      '<span class="g-acts"><a href="' + C.esc(C.xmlUrl(t, m[3])) + '" target="_blank" rel="noopener" title="TEI source">XML l.' + m[3] + '</a>' +
      '<button type="button" class="linkish cite-seg" title="Copy citation">cite</button></span></div>' +
      '<div class="stext">' + html + '</div></div>';
  }

  function render(scrollToTarget) {
    const script = $('#r-script').value, showPages = $('#r-pages').checked, showNotes = $('#r-notes').checked;
    let html = '', lastCtx = -1;
    for (let i = from; i < to; i++) {
      if (!showNotes && (kinds[i] === 'n' || kinds[i] === 'v')) continue;
      const c = meta.segs[i][0];
      if (c !== lastCtx) {
        lastCtx = c;
        if (meta.ctx[c]) html += '<div class="ctx-bar">' + C.esc(meta.ctx[c]) + '</div>';
      }
      html += segHTML(i, script, showPages);
    }
    $('#r-body').innerHTML = html;
    $('#r-prev').hidden = from <= 0;
    $('#r-next').hidden = to >= lines.length;
    if (scrollToTarget) {
      const el = document.getElementById('s' + target);
      if (el) el.scrollIntoView({ block: 'center' });
    }
  }

  function show(i) {
    target = Math.max(0, Math.min(lines.length - 1, i));
    from = Math.max(0, target - 40);
    to = Math.min(lines.length, target + CHUNK);
    history.replaceState(null, '', '?t=' + encodeURIComponent(id) + '&s=' + target + '#s' + target);
    render(true);
  }

  $('#r-prev').addEventListener('click', () => {
    const anchor = document.getElementById('s' + from);
    const y = anchor ? anchor.getBoundingClientRect().top : 0;
    from = Math.max(0, from - CHUNK); render(false);
    if (anchor) { const a2 = document.getElementById(anchor.id); if (a2) window.scrollBy(0, a2.getBoundingClientRect().top - y); }
  });
  $('#r-next').addEventListener('click', () => { to = Math.min(lines.length, to + CHUNK); render(false); });
  ['#r-script', '#r-pages', '#r-notes'].forEach(s => $(s).addEventListener('change', () => render(false)));

  $('#goto').addEventListener('submit', e => {
    e.preventDefault();
    const q = $('#goto-ref').value.trim().toLowerCase();
    if (!q) return;
    const eq = r => r && r.toLowerCase() === q;
    const ends = r => r && (r.toLowerCase().endsWith(q) || r.toLowerCase().endsWith('.' + q));
    const has = r => r && r.toLowerCase().includes(q);
    for (const test of [eq, ends, has]) {
      for (let i = 0; i < meta.segs.length; i++) {
        const m = meta.segs[i];
        if (test(m[1]) || (m[4] || []).some(x => x[1] === 'r' && test(x[2]))) { show(i); status(''); return; }
      }
    }
    status('No passage with a reference matching “' + C.esc(q) + '”.', true);
  });

  $('#r-body').addEventListener('click', e => {
    const b = e.target.closest('.cite-seg'); if (!b) return;
    const i = +b.closest('.seg').dataset.i, m = meta.segs[i];
    const ref = m[1] || ((m[4] || []).find(x => x[1] === 'r') || [])[2] || '';
    C.copyText(C.citation(t, { ref, ctx: meta.ctx[m[0]], loc: m[2], line: m[3], kind: kinds[i] }), b);
  });

  async function boot() {
    if (!id) { $('#r-head').innerHTML = '<p>No text chosen. See the <a href="' + S.base + 'texts.html">list of texts</a>.</p>'; return; }
    try {
      const cat = await C.catalogue();
      t = cat.byId.get(id);
      if (!t) throw new Error('Unknown text “' + id + '”.');
      header();
      const [txt, m] = await Promise.all([
        fetch(S.base + 'data/t/' + encodeURIComponent(id) + '.txt').then(r => { if (!r.ok) throw new Error('text not found'); return r.text(); }),
        fetch(S.base + 'data/m/' + encodeURIComponent(id) + '.json').then(r => { if (!r.ok) throw new Error('citation data not found'); return r.json(); })
      ]);
      meta = m;
      const all = txt.split('\n'); if (all[all.length - 1] === '') all.pop();
      lines = all.map(l => l.slice(1)); kinds = all.map(l => l[0]).join('');
      $('#r-tools').hidden = false;
      show(target);
    } catch (e) { status(C.esc(e.message || e), true); }
  }
  boot();
})();
