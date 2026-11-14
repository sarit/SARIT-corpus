/* SARIT search — shared helpers (catalogue, citations, display). */
(function (G) {
  'use strict';
  const S = G.SARIT_SITE || { base: '/', repo: '', branch: 'main' };
  const T = G.SaritTranslit;

  let catPromise = null;
  function catalogue() {
    if (!catPromise) {
      catPromise = fetch(S.base + 'data/catalogue.json').then(r => {
        if (!r.ok) throw new Error('The text catalogue could not be loaded (' + r.status + '). Has the data been built?');
        return r.json();
      }).then(list => {
        const byId = new Map();
        list.forEach((t, i) => { t.order = i; byId.set(t.id, t); });
        return { list, byId };
      });
    }
    return catPromise;
  }

  function esc(s) {
    return String(s == null ? '' : s).replace(/[&<>"']/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
  }

  function xmlUrl(t, line) {
    const path = t.file.split('/').map(encodeURIComponent).join('/');
    return S.repo + '/blob/' + S.branch + '/' + path + (line ? '#L' + line : '');
  }
  function readerUrl(id, seg) {
    return S.base + 'read.html?t=' + encodeURIComponent(id) + (seg != null ? '&s=' + seg + '#s' + seg : '');
  }

  function authorsOf(t) { return (t.authors || []).filter(Boolean).join(', '); }

  function locLabel(loc) { return loc ? 'ed. ' + loc : ''; }

  // A citation string for a hit or segment.
  function citation(t, h) {
    const today = new Date().toISOString().slice(0, 10);
    const parts = [];
    let head = t.title;
    const au = authorsOf(t);
    if (au) head = au + ', ' + head;
    parts.push(head);
    if (h.ref) parts.push(h.ref);
    if (h.ctx) parts.push('[' + h.ctx + ']');
    if (h.loc) parts.push(locLabel(h.loc));
    if (h.kind === 'n') parts.push('(note)');
    if (h.kind === 'v') parts.push('(variant reading)');
    let s = parts.join(', ') + '.';
    if (t.source && t.source.length) s += ' Source: ' + t.source[0].replace(/\.$/, '') + '.';
    s += ' ' + (t.coll === 'GRETIL' ? 'GRETIL e-text in the SARIT corpus' : 'SARIT e-text') + ', ' + t.file + ', line ' + h.line + ', ' + xmlUrl(t, h.line) + ' (accessed ' + today + ').';
    return s;
  }

  // Render text (with optional highlight ranges) in the chosen script.
  function renderText(text, hls, script) {
    hls = hls || [];
    const pieces = [];
    let p = 0;
    for (const [a, b] of hls) { pieces.push([text.slice(p, a), false]); pieces.push([text.slice(a, b), true]); p = b; }
    pieces.push([text.slice(p), false]);
    const conv = (s) => {
      if (script === 'iast') return T.devaToIAST(s);
      if (script === 'deva') return T.hasDeva(s) ? s : T.iastToDeva(s);
      return s;
    };
    return pieces.map(([s, m]) => {
      if (!s) return '';
      const h = esc(conv(s)).replace(/\u2028/g, '\n');
      return m ? '<mark>' + h + '</mark>' : h;
    }).join('');
  }

  function scriptClass(text, script) {
    if (script === 'deva') return 'deva';
    if (script === 'iast') return 'latn';
    return T.hasDeva(text) ? 'deva' : 'latn';
  }

  async function copyText(s, btn) {
    try {
      await navigator.clipboard.writeText(s);
    } catch (e) {
      const ta = document.createElement('textarea');
      ta.value = s; document.body.appendChild(ta); ta.select();
      try { document.execCommand('copy'); } catch (e2) { }
      ta.remove();
    }
    if (btn) { const o = btn.textContent; btn.textContent = 'Copied'; setTimeout(() => btn.textContent = o, 1200); }
  }

  function fmtNum(n) { return n.toLocaleString('en'); }
  function fmtSize(chars) {
    if (chars > 1e6) return (chars / 1e6).toFixed(1) + ' M chars';
    if (chars > 1e3) return Math.round(chars / 1e3) + ' k chars';
    return chars + ' chars';
  }

  G.SaritCommon = { S, catalogue, esc, xmlUrl, readerUrl, citation, renderText, scriptClass, copyText, fmtNum, fmtSize, authorsOf, locLabel };
})(self);
