# SARIT corpus search site

A Jekyll site that searches every TEI file in this repository (the SARIT texts in
the root folder and the GRETIL texts in `GRETIL-corpustei/`). It is published with
GitHub Pages at <https://wujastyk.github.io/SARIT-corpus/>.

## How it works

1. `_tools/extract.py` reads each TEI file and writes
   * `data/catalogue.json` – title, author, source edition, licence etc. of every text;
   * `data/t/<id>.txt` – the text, one passage (verse, paragraph, note …) per line;
   * `data/m/<id>.json` – the citation data for each passage: division hierarchy,
     reference (`xml:id`, `@n` or `<label>`), page/line of the printed edition
     (`<pb/>`, `<lb/>`), line number in the XML file, and sub-references inside
     the passage (half-verse ids, anchors, page breaks).
2. The pages (`index.html`, `texts.html`, `read.html`, `help.md`) load that data in
   the browser. Searching is done in web workers (`assets/js/search-worker.js`);
   `assets/js/translit.js` converts texts and queries to a common normalised IAST
   form, so IAST and Devanagari match each other.
3. `.github/workflows/search-site.yml` runs the extractor and Jekyll on every push
   to `main` and deploys the result. The `data/` folder is generated, not committed.

## One-time setup on GitHub

Settings → Pages → Build and deployment → Source: **GitHub Actions**.
The workflow file must be in `.github/workflows/` on the `main` branch.

## Local preview

```sh
pip install lxml
python3 website/_tools/extract.py --repo . --out website/data   # ~1–2 minutes
cd website
bundle install
bundle exec jekyll serve      # then open http://localhost:4000/SARIT-corpus/
```

## Adjusting citation numbers

`_tools/citation-config.json` can hold per-file settings, keyed by file name:

```json
{
  "mahabharata_digital_concordance-bori.xml": { "citePrefix": "MBh ", "skipDivTypes": ["upaparva"] }
}
```

* `skipDivTypes` – `div/@type` values left out when a reference is built from
  `@n` attributes (default: `upaparva`, `subparva`).
* `citePrefix` – text put before such references.
* `_skipFiles` – files that are not indexed.

## Files with problems

The extractor prints warnings in the Actions log. At the time of writing, ten
GRETIL files have stray characters between the XML declaration and the root
element (e.g. `sa_kaṭhināvadāna.xml`); they are indexed anyway, but should be
fixed in the XML.
