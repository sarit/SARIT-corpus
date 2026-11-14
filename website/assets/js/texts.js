/* SARIT search — catalogue page. */
(function () {
  'use strict';
  const T = self.SaritTranslit, C = self.SaritCommon, S = C.S;
  const $ = s => document.querySelector(s);
  let CAT = null, keys = [];
  function render() {
    const f = T.fastKey($('#t-filter').value.trim(), { noDiac: true });
    const coll = document.querySelector('input[name=tcoll]:checked').value;
    const rows = CAT.list.filter((t, i) => (coll === 'all' || t.coll === coll) && (!f || keys[i].includes(f)));
    $('#t-count').textContent = rows.length + ' of ' + CAT.list.length + ' texts';
    $('#t-body').innerHTML = rows.map(t =>
      '<tr><td><a href="' + C.esc(C.readerUrl(t.id, 0)) + '" class="t-title">' + C.esc(t.title) + '</a>' +
      (t.subtitle ? '<div class="muted small">' + C.esc(t.subtitle) + '</div>' : '') +
      '<div class="muted small">' + C.esc(t.file) + ' <span class="badge badge-' + t.coll.toLowerCase() + '">' + t.coll + '</span></div></td>' +
      '<td>' + C.esc(C.authorsOf(t)) + '</td>' +
      '<td class="small">' + C.esc((t.source && t.source[0]) || (t.gretilHeader ? t.gretilHeader.slice(1, 3).join(' ') : '')) + '</td>' +
      '<td>' + ({ Deva: 'Devanagari', Latn: 'Roman', mixed: 'mixed' }[t.script] || '') + '</td>' +
      '<td class="num">' + C.fmtSize(t.chars) + '</td>' +
      '<td class="t-acts"><a href="' + S.base + '?coll=sel&texts=' + encodeURIComponent(t.id) + '">search</a> ' +
      '<a href="' + C.esc(C.xmlUrl(t)) + '">XML</a></td></tr>').join('');
  }
  C.catalogue().then(cat => {
    CAT = cat;
    keys = cat.list.map(t => T.fastKey([t.title, t.subtitle, C.authorsOf(t), t.file, (t.source || []).join(' '), (t.gretilHeader || []).join(' ')].join(' '), { noDiac: true }));
    render();
  }).catch(e => { $('#t-body').innerHTML = '<tr><td colspan="6" class="error">' + C.esc(e.message) + '</td></tr>'; });
  $('#t-filter').addEventListener('input', () => CAT && render());
  document.querySelectorAll('input[name=tcoll]').forEach(r => r.addEventListener('change', render));
})();
