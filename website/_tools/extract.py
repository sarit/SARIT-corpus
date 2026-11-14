#!/usr/bin/env python3
"""
Extract searchable, citable text segments from the TEI files of the
SARIT corpus and write them as JSON for the Jekyll search site.

Usage:  python3 website/_tools/extract.py  [--repo .] [--out website/data]

Output
  data/catalogue.json      one record per text (bibliographic metadata)
  data/t/<slug>.txt        the text, one segment per line; the first character of
                           each line is its kind ('.' text, 'h' heading, 'n' note,
                           'v' variant reading)
  data/m/<slug>.json       citation data, one entry per line of the .txt file:
                           [ctx, ref, loc, xmlLine, marks?]

Internally each segment is a list:
  [ctx, ref, loc, xmlLine, kind, text, marks]
    ctx      index into the text's "ctx" table (div hierarchy, e.g. "adhyāya 1: …")
    ref      canonical reference of the block (xml:id, @n path or <label>), or ""
    loc      printed-edition location at the start of the block (page.line)
    xmlLine  line number in the XML source file (for a GitHub link)
    kind     "" main text, "h" heading, "n" note, "v" variant reading
    text     the text, whitespace-normalised; U+2028 separates verse lines
    marks    [[offset, "p", loc] | [offset, "r", ref], ...]  changes of page/line
             or of sub-reference (e.g. half-verse ids) inside the segment
"""
import argparse, glob, json, os, re, sys, unicodedata
from collections import OrderedDict, Counter
from lxml import etree

TEI = '{http://www.tei-c.org/ns/1.0}'
XMLID = '{http://www.w3.org/XML/1998/namespace}id'
XMLLANG = '{http://www.w3.org/XML/1998/namespace}lang'
LSEP = '\u2028'

BLOCKS = {'p', 'ab', 'lg', 'head', 'item', 'trailer', 'byline', 'dateline', 'closer',
          'opener', 'salute', 'signed', 'row', 'docTitle', 'titlePart', 'docAuthor',
          'docImprint', 'docEdition', 'speaker', 'argument', 'epigraph', 'l'}
SKIP = {'teiHeader', 'figDesc', 'fw', 'del', 'witDetail', 'interpGrp', 'spanGrp',
        'linkGrp', 'facsimile', 'ptr', 'graphic', 'certainty', 'respons', 'index'}
DIVS = {'div', 'div1', 'div2', 'div3', 'div4', 'div5', 'div6', 'div7'}
CHOICE_PREF = ['corr', 'reg', 'expan', 'lem', 'sic', 'orig', 'abbr', 'seg']
# div types that are not normally part of a citation number
DIV_TYPES_NOT_CITED = {'upaparva', 'upaparvan', 'subparva', 'section-group', 'group'}


def local(tag):
    return tag.split('}', 1)[1] if isinstance(tag, str) and '}' in tag else tag


def ws(s):
    return re.sub(r'\s+', ' ', s or '').strip()


def text_of(el, skip=('note', 'label', 'fw', 'del', 'sic', 'orig', 'rdg')):
    """Plain text of an element (for headings, metadata)."""
    out = []

    def rec(e):
        if e is not el and isinstance(e.tag, str) and local(e.tag) in skip:
            if e.tail:
                out.append(e.tail)
            return
        if e.text and isinstance(e.tag, str):
            out.append(e.text)
        for c in e:
            rec(c)
        if e.tail and e is not el:
            out.append(e.tail)
    rec(el)
    return ws(''.join(out))


def strip0(n):
    n = n.strip()
    return re.sub(r'^0+(?=\d)', '', n) if re.fullmatch(r'\d+', n) else n


def slugify(s):
    s = unicodedata.normalize('NFD', s)
    s = ''.join(c for c in s if not unicodedata.combining(c))
    s = re.sub(r'[^A-Za-z0-9_-]+', '-', s).strip('-').lower()
    return s or 'text'


# --------------------------------------------------------------------------
# metadata
# --------------------------------------------------------------------------
def header_meta(root):
    h = root.find('.//' + TEI + 'teiHeader')
    m = OrderedDict()
    if h is None:
        return m
    ts = h.find('.//' + TEI + 'titleStmt')
    if ts is not None:
        titles = ts.findall(TEI + 'title')
        main = [t for t in titles if t.get('type') == 'main'] or titles[:1]
        if main:
            m['title'] = text_of(main[0])
        subs = [text_of(t) for t in titles if t.get('type') == 'sub' and text_of(t)]
        subs = [s for s in subs if s not in ('A SARIT edition',)]
        if subs:
            m['subtitle'] = '; '.join(subs)
        au = [text_of(a) for a in ts.findall(TEI + 'author') if text_of(a)]
        if au:
            m['authors'] = au
        resp = []
        for r in ts.findall(TEI + 'respStmt'):
            who = [text_of(x) for x in r if local(x.tag) in ('persName', 'name', 'orgName') and text_of(x)]
            what = r.find(TEI + 'resp')
            if who and what is not None:
                resp.append('%s: %s' % (text_of(what), ', '.join(who)))
        if resp:
            m['resp'] = resp[:6]
    sd = h.find('.//' + TEI + 'sourceDesc')
    if sd is not None:
        bibls = sd.findall('.//' + TEI + 'bibl') + sd.findall('.//' + TEI + 'biblStruct')
        src = []
        for b in bibls[:3]:
            if b.getparent() is not None and local(b.getparent().tag) in ('bibl', 'biblStruct'):
                continue
            parts = []
            tt = b.find('.//' + TEI + 'title')
            if tt is not None and text_of(tt):
                parts.append(text_of(tt))
            for tag, lab in (('author', ''), ('editor', 'ed. '), ('publisher', ''), ('pubPlace', ''), ('date', '')):
                vals = [text_of(x) for x in b.findall('.//' + TEI + tag) if text_of(x)]
                if tag == 'author' and tt is None:
                    pass
                if vals:
                    parts.append(lab + ', '.join(vals))
            s = '. '.join(parts) if parts else text_of(b)
            if s:
                src.append(s[:400])
        if not src:
            s = text_of(sd)
            if s and not s.startswith('This file was born digitally'):
                src.append(s[:400])
        if src:
            m['source'] = src
    # GRETIL legacy header and original URL
    for n in h.iter(TEI + 'note'):
        if n.get('type') == 'legacyheader':
            lines = [ws(x) for x in re.split(r'\n', ''.join(n.itertext())) if ws(x)]
            m['gretilHeader'] = lines[:12]
    for r in h.iter(TEI + 'ref'):
        t = r.get('target') or ''
        if 'gretil' in t and t.startswith('http'):
            m['gretilUrl'] = t
            break
    av = h.find('.//' + TEI + 'availability')
    if av is not None:
        lic = av.find('.//' + TEI + 'licence')
        if lic is not None:
            m['licence'] = lic.get('target') or text_of(lic)
        else:
            for r in av.iter(TEI + 'ref'):
                if r.get('type') in ('licence', 'license') or 'creativecommons' in (r.get('target') or ''):
                    m['licence'] = r.get('target')
                    break
    langs = [l.get('ident') for l in h.iter(TEI + 'language') if l.get('ident')]
    if langs:
        m['langs'] = langs
    return m


# --------------------------------------------------------------------------
# segment extraction
# --------------------------------------------------------------------------
class Seg:
    __slots__ = ('ctx', 'ref', 'loc', 'line', 'kind', 'buf', 'n', 'marks', 'order')

    def __init__(self, ctx, ref, loc, line, kind, order):
        self.ctx, self.ref, self.loc, self.line, self.kind = ctx, ref or '', loc, line, kind
        self.buf, self.n, self.marks, self.order = [], 0, [], order

    def last(self):
        return self.buf[-1][-1] if self.buf else ''

    def add(self, s, raw=False):
        if not s:
            return
        if not raw:
            s = re.sub(r'\s+', ' ', s)
            if s.startswith(' ') and (self.n == 0 or self.last() in (' ', LSEP)):
                s = s[1:]
        if s:
            self.buf.append(s)
            self.n += len(s)

    def rstrip(self):
        while self.buf and self.buf[-1].endswith(' '):
            self.buf[-1] = self.buf[-1][:-1]
            self.n -= 1
            if not self.buf[-1]:
                self.buf.pop()

    def text(self):
        t = ''.join(self.buf)
        return t

    def mark(self, typ, val):
        off = self.n
        if self.marks and self.marks[-1][0] == off and self.marks[-1][1] == typ:
            self.marks[-1][2] = val
        else:
            self.marks.append([off, typ, val])


class Extractor:
    def __init__(self, root, cfg):
        self.cfg = cfg
        self.root = root
        self.ctxs = OrderedDict()
        self.divstack = []          # [(label, n, type)]
        self.segs = []              # finished Seg objects
        self.stack = []             # open Seg objects (innermost last)
        self.order = 0
        self.lastline = 0
        self.pb = OrderedDict()     # ed -> page
        self.lb = OrderedDict()     # ed -> line
        self.lbcount = Counter()
        eds = set()
        for pb in root.iter(TEI + 'pb'):
            eds.add(pb.get('ed') or pb.get('edRef') or '')
        self.multi_ed = len(eds) > 1
        self.lb_numbered = any(lb.get('n') for lb in root.iter(TEI + 'lb'))
        self.skip_types = set(cfg.get('skipDivTypes', DIV_TYPES_NOT_CITED))
        self.cite_prefix = cfg.get('citePrefix', '')

    # ---- context helpers
    def ctx_index(self):
        key = ' › '.join(d[0] for d in self.divstack if d[0])
        if key not in self.ctxs:
            self.ctxs[key] = len(self.ctxs)
        return self.ctxs[key]

    def npath(self, n):
        parts = [strip0(d[1]) for d in self.divstack if d[1] and d[2] not in self.skip_types]
        parts.append(strip0(n))
        return self.cite_prefix + '.'.join(parts)

    def loc(self):
        out = []
        for ed in list(self.pb.keys()) or list(self.lb.keys()):
            p = self.pb.get(ed, '')
            l = self.lb.get(ed, '')
            s = p + ('.' + l if p and l else ('l.' + l if l else ''))
            if s:
                edl = ed.lstrip('#')
                out.append((edl + ' ' if self.multi_ed and edl else '') + s)
        return '; '.join(out)

    # ---- segment helpers
    def open(self, el, kind='', ref=''):
        self.order += 1
        s = Seg(self.ctx_index(), ref, self.loc(), el.sourceline or 0, kind, self.order)
        self.stack.append(s)
        return s

    def close(self):
        s = self.stack.pop()
        s.rstrip()
        if s.n and s.text().strip():
            self.segs.append(s)
        return s

    @property
    def cur(self):
        return self.stack[-1] if self.stack else None

    def block_ref(self, el):
        i = el.get(XMLID)
        if i:
            return i
        n = el.get('n')
        if n and local(el.tag) in ('lg', 'p', 'ab', 'item', 'l'):
            return self.npath(n)
        return ''

    # ---- walker
    def emit(self, s):
        if not s:
            return
        if self.cur is None:
            # loose text directly in a div: open an anonymous segment
            if not s.strip():
                return
            self.open_anon()
        self.cur.add(s)

    def open_anon(self):
        self.order += 1
        seg = Seg(self.ctx_index(), '', self.loc(), self.lastline, '', self.order)
        self.stack.append(seg)

    def walk(self, el):
        tag = el.tag
        if not isinstance(tag, str):
            self.emit(el.tail)
            return
        t = local(tag)
        if el.sourceline:
            self.lastline = el.sourceline
        if t in SKIP:
            self.emit(el.tail)
            return

        if t in DIVS or (t in ('front', 'back', 'body', 'group', 'text') and el is not self.root):
            self.flush_anon()
            if t in DIVS:
                head = el.find(TEI + 'head')
                ht = text_of(head)[:70] if head is not None else ''
                typ = el.get('subtype') or el.get('type') or ''
                if re.fullmatch(r'(level|div|textpart|part)\d*', typ):
                    typ = ''
                n = el.get('n') or ''
                lab = ' '.join(x for x in (typ, strip0(n)) if x)
                lab = (lab + ': ' + ht) if (lab and ht) else (lab or ht)
                self.divstack.append((lab, n, typ))
            if el.text and el.text.strip():
                self.emit(el.text)
            for c in el:
                self.walk(c)
            self.flush_anon()
            if t in DIVS:
                self.divstack.pop()
            self.emit(el.tail)
            return

        if t == 'pb':
            ed = el.get('ed') or el.get('edRef') or ''
            self.pb[ed] = el.get('n') or '?'
            if ed in self.lb:
                self.lb[ed] = ''
            self.lbcount[ed] = 0
            if self.cur is not None:
                self.cur.mark('p', self.loc())
            self.emit(el.tail)
            return
        if t == 'lb':
            ed = el.get('ed') or el.get('edRef') or ''
            self.lbcount[ed] += 1
            self.lb[ed] = strip0(el.get('n')) if el.get('n') else (str(self.lbcount[ed]) if not self.lb_numbered else self.lb.get(ed, ''))
            if self.cur is not None:
                self.cur.mark('p', self.loc())
            tail = el.tail or ''
            if el.get('break') == 'no' and self.cur is not None:
                self.cur.rstrip()
                tail = tail.lstrip()
            self.emit(tail)
            return
        if t in ('anchor', 'milestone'):
            val = el.get(XMLID) if t == 'anchor' else (el.get('n') if el.get('unit') not in ('speaker', None) else None)
            if t == 'milestone' and el.get('unit') and el.get('n') and el.get('unit') != 'speaker':
                val = el.get('n')
            if val and self.cur is not None:
                self.cur.mark('r', val)
            self.emit(el.tail)
            return
        if t == 'gap':
            self.emit(' … ')
            self.emit(el.tail)
            return
        if t == 'label':
            lt = text_of(el)
            cur = self.cur
            if lt and len(lt) <= 40 and cur is not None:
                has_r = any(m[1] == 'r' for m in cur.marks)
                if not cur.ref and not has_r and cur.n == 0:
                    cur.ref = lt
                elif not (cur.marks and cur.marks[-1][1] == 'r'):
                    cur.mark('r', lt)
            elif lt:
                self.emit(' ' + lt + ' ')
            self.emit(el.tail)
            return
        if t in ('note', 'rdg') or (t == 'rdgGrp'):
            if t == 'rdg' and el.getparent() is not None and el.getparent().find(TEI + 'lem') is None \
                    and el.getparent().find(TEI + 'rdg') is el:
                # apparatus without lemma: treat first reading as the text
                self.inline(el)
                return
            parent_ref = ''
            if self.cur is not None:
                rs = [m[2] for m in self.cur.marks if m[1] == 'r']
                parent_ref = rs[-1] if rs else self.cur.ref
            nref = el.get('n') or el.get(XMLID) or ''
            if t == 'rdg':
                wit = el.get('wit') or ''
                nref = (wit.replace('#', '') + ' ') if wit else ''
            seg = self.open(el, 'n' if t == 'note' else 'v',
                            (parent_ref + (' n.' + nref if t == 'note' and nref else '')).strip() if parent_ref else ('n.' + nref if nref else ''))
            if t == 'rdg' and nref:
                seg.add(nref.strip() + ': ')
            if el.text:
                seg.add(el.text)
            for c in el:
                self.walk(c)
            self.close()
            self.emit(el.tail)
            return
        if t == 'choice':
            kids = {local(c.tag): c for c in el if isinstance(c.tag, str)}
            for p in CHOICE_PREF:
                if p in kids:
                    self.inline(kids[p], tail=False)
                    break
            self.emit(el.tail)
            return
        if t == 'app':
            lem = el.find(TEI + 'lem')
            if lem is not None:
                self.inline(lem, tail=False)
            for c in el:
                if isinstance(c.tag, str) and local(c.tag) in ('rdg', 'note', 'rdgGrp'):
                    self.walk(c)
                    # tails of rdg are whitespace; ignore
            self.emit(el.tail)
            return

        # l inside lg: line separator + sub-reference
        if t == 'l' and el.getparent() is not None and local(el.getparent().tag) in ('lg',) and self.cur is not None:
            cur = self.cur
            if cur.n:
                cur.rstrip()
                cur.add(LSEP, raw=True)
            r = el.get(XMLID) or ''
            if not r and el.get('n'):
                n = el.get('n')
                base = cur.ref or ''
                r = (base + n) if (base and len(n) <= 2 and n.isalpha()) else self.npath(n) if not base else base + '.' + n
            if r:
                cur.mark('r', r)
            self.inline(el)
            return
        if t == 'lg' and self.cur is not None and local(el.getparent().tag) == 'lg':
            r = el.get(XMLID) or (self.npath(el.get('n')) if el.get('n') else '')
            if r:
                self.cur.mark('r', r)
            self.inline(el)
            return

        if t in BLOCKS or (t == 'quote' and el.find(TEI + 'p') is not None and False):
            outer = self.cur
            resume = None
            if outer is not None:
                resume = (outer.kind, outer.ref)
                self.close_split()
            kind = 'h' if t == 'head' else (outer.kind if outer is not None and outer.kind in ('n', 'v') else '')
            if t == 'head' and el.getparent() is not None and local(el.getparent().tag) in DIVS:
                kind = 'h'
            ref = self.block_ref(el)
            if not ref and resume and resume[0] in ('n', 'v'):
                ref = resume[1]
            self.open(el, kind, ref)
            if el.text:
                self.cur.add(el.text)
            for c in el:
                self.walk(c)
            self.close()
            if resume is not None:
                self.order += 1
                s = Seg(self.ctx_index(), resume[1], self.loc(), self.lastline, resume[0], self.order)
                self.stack.append(s)
            self.emit(el.tail)
            return

        # generic inline element
        self.inline(el)

    def inline(self, el, tail=True):
        if el.text:
            self.emit(el.text)
        for c in el:
            self.walk(c)
        if tail:
            self.emit(el.tail)

    def close_split(self):
        """Close the innermost open segment because a nested block starts."""
        self.close()

    def flush_anon(self):
        while self.stack:
            self.close()

    def run(self):
        text = self.root.find(TEI + 'text')
        if text is None:
            text = self.root.find('.//' + TEI + 'text')
        if text is None:
            return []
        for part in text:
            if isinstance(part.tag, str):
                self.walk(part)
        self.flush_anon()
        self.segs.sort(key=lambda s: s.order)
        out = []
        for s in self.segs:
            txt = s.text()
            marks = [m for m in s.marks if m[0] < len(txt)]
            # drop redundant loc marks at offset 0 (the seg loc covers it)
            if marks and marks[0][0] == 0 and marks[0][1] == 'p':
                s.loc = marks[0][2]
                marks = marks[1:]
            # collapse repeated identical consecutive values
            cleaned, lastv = [], {'p': s.loc, 'r': None}
            for m in marks:
                if lastv.get(m[1]) == m[2]:
                    continue
                lastv[m[1]] = m[2]
                cleaned.append(m)
            rec = [s.ctx, s.ref, s.loc, s.line, s.kind, txt]
            if cleaned:
                rec.append(cleaned)
            out.append(rec)
        return out


def detect_script(segs):
    dev = lat = 0
    for s in segs[:4000]:
        for ch in s[5][:400]:
            o = ord(ch)
            if 0x900 <= o <= 0x97f:
                dev += 1
            elif ch.isalpha():
                lat += 1
    if dev + lat == 0:
        return ''
    r = dev / (dev + lat)
    return 'Deva' if r > 0.7 else ('Latn' if r < 0.3 else 'mixed')


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--repo', default='.')
    ap.add_argument('--out', default='website/data')
    ap.add_argument('--only', default='')
    a = ap.parse_args()
    repo = os.path.abspath(a.repo)
    cfgpath = os.path.join(repo, 'website', '_tools', 'citation-config.json')
    cfgall = json.load(open(cfgpath, encoding='utf-8')) if os.path.exists(cfgpath) else {}
    os.makedirs(os.path.join(a.out, 't'), exist_ok=True)
    os.makedirs(os.path.join(a.out, 'm'), exist_ok=True)
    files = sorted(glob.glob(os.path.join(repo, '*.xml'))) + \
        sorted(glob.glob(os.path.join(repo, 'GRETIL-corpustei', '*.xml')))
    skipnames = set(cfgall.get('_skipFiles', ['saritcorpus.xml', '00-sarit-tei-header-template.xml']))
    cat, slugs = [], set()
    for f in files:
        rel = os.path.relpath(f, repo)
        base = os.path.basename(f)
        if base in skipnames or (a.only and a.only not in rel):
            continue
        try:
            parser = etree.XMLParser(huge_tree=True, recover=True, remove_comments=True,
                                     remove_pis=True, resolve_entities=False, no_network=True)
            tree = etree.parse(f, parser)
        except Exception as e:
            print('!! cannot parse', rel, e, file=sys.stderr)
            continue
        root = tree.getroot()
        if root is None or local(root.tag) != 'TEI':
            raw = open(f, 'rb').read()
            fixed = re.sub(rb'^(\s*<\?xml[^>]*\?>)[^<]+', rb'\1\n', raw)
            if fixed != raw:
                print('!! stray characters before the root element (please fix in the XML):', rel, file=sys.stderr)
                tree = etree.ElementTree(etree.fromstring(fixed, parser))
                root = tree.getroot()
        if root is None or local(root.tag) != 'TEI':
            print('-- skip (not a TEI document)', rel, file=sys.stderr)
            continue
        cfg = cfgall.get(base, {})
        meta = header_meta(root)
        x = Extractor(root, cfg)
        try:
            segs = x.run()
        except RecursionError:
            print('!! recursion', rel, file=sys.stderr)
            continue
        if not segs:
            print('-- no text', rel, file=sys.stderr)
            continue
        slug = slugify(os.path.splitext(base)[0])
        while slug in slugs:
            slug += '-x'
        slugs.add(slug)
        ctx = list(x.ctxs.keys())
        lang = root.get(XMLLANG) or ''
        t = root.find(TEI + 'text')
        if t is not None and t.get(XMLLANG):
            lang = t.get(XMLLANG)
        rec = OrderedDict()
        rec['id'] = slug
        rec['file'] = rel.replace(os.sep, '/')
        rec['coll'] = 'GRETIL' if rel.startswith('GRETIL') else 'SARIT'
        rec.update(meta)
        if not rec.get('title'):
            rec['title'] = os.path.splitext(base)[0]
        rec['lang'] = lang or (meta.get('langs') or [''])[0]
        rec['script'] = detect_script(segs)
        rec['segs'] = len(segs)
        rec['chars'] = sum(len(s[5]) for s in segs)
        if cfg.get('citePrefix'):
            rec['citePrefix'] = cfg['citePrefix']
        cat.append(rec)
        # t/<slug>.txt : one segment per line, first character = kind ('.', 'h', 'n', 'v')
        with open(os.path.join(a.out, 't', slug + '.txt'), 'w', encoding='utf-8', newline='\n') as fh:
            for sg in segs:
                fh.write((sg[4] or '.') + sg[5].replace('\n', ' ').replace('\r', ' ') + '\n')
        # m/<slug>.json : citation data, parallel to the lines of the .txt file
        meta_segs = [[sg[0], sg[1], sg[2], sg[3]] + ([sg[6]] if len(sg) > 6 else []) for sg in segs]
        with open(os.path.join(a.out, 'm', slug + '.json'), 'w', encoding='utf-8') as fh:
            json.dump({'id': slug, 'ctx': ctx, 'segs': meta_segs}, fh, ensure_ascii=False, separators=(',', ':'))
        print('%-60s %7d segs %9d chars %s' % (rel[:60], len(segs), rec['chars'], rec['script']), file=sys.stderr)
    cat.sort(key=lambda r: (r['coll'] != 'SARIT', unicodedata.normalize('NFD', r['title']).lower()))
    with open(os.path.join(a.out, 'catalogue.json'), 'w', encoding='utf-8') as fh:
        json.dump(cat, fh, ensure_ascii=False, separators=(',', ':'))
    print('texts:', len(cat), 'chars:', sum(r['chars'] for r in cat), file=sys.stderr)


if __name__ == '__main__':
    sys.setrecursionlimit(20000)
    main()
