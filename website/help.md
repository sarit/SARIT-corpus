---
layout: default
title: Help
slug: help
---
<div class="prose" markdown="1">

# How to search

Type a word or phrase and press **Search**. The search runs in your browser over
the TEI files of this repository: the SARIT texts and the GRETIL texts in
`GRETIL-corpustei/`.

## Scripts and transliteration

You can type in **IAST** (`dharmakṣetre`), **Devanagari** (`धर्मक्षेत्रे`) or
plain ASCII. It does not matter which script the text is encoded in: before
searching, both the texts and your query are converted to one form, which is
lower-case IAST with the following unifications:

| In the text or query | is treated as |
|---|---|
| Devanagari | IAST (क्ष → kṣa, ं → ṃ, ः → ḥ, ऽ → ') |
| ISO 15919 `ṁ`, `r̥`, `l̥`, `ē`, `ō` | `ṃ`, `ṛ`, `ḷ`, `e`, `o` |
| `/`, `।` and `//`, `॥` | the same single and double daṇḍa |
| `’`, `‘`, `ऽ` (avagraha) | `'` |
| Devanagari digits | 0–9 |
| Vedic accents, upper case | ignored |

So `dharmakṣetre`, `धर्मक्षेत्रे` and `DHARMAKṢETRE` find the same passages.
Under **More options** you can also type in Harvard-Kyoto, Velthuis, SLP1 or
ITRANS. The row of buttons under the search box inserts IAST letters.

## Match modes

* **Phrase** — the words exactly as typed, in that order (as a substring: `jīvit`
  finds *jīvitam*, *jīvitasya* …).
* **All words** — every word must occur in the same passage (a verse, paragraph,
  note or heading), in any order.
* **Any word** — at least one of the words.
* **Regex (grep)** — a JavaScript regular expression. It is matched against the
  normalised IAST form of each passage, one passage per line, so:
  * `^` and `$` mark the start and end of a passage;
  * `.` is any character; a daṇḍa is `\|`;
  * Devanagari and IAST in the pattern are both converted, e.g. `vāt.*pitt` or
    `(ś|s)arīr`;
  * `\p{L}` (any letter) and other Unicode classes work;
  * there is no need to put `.*` at the start or end of a pattern: `yatra`
    already finds every passage that contains *yatra* anywhere. A leading or
    trailing `.*` is removed before searching, because on very long passages it
    makes the browser's regular-expression engine run out of memory;
  * `AND` and `OR` (in capitals, with spaces round them) combine patterns as
    below, e.g. `yat(ra|o) AND tat(ra|as)`.

### AND, OR and wildcards

In any mode you can combine search terms with `AND` or `OR`, written in
capitals:

* `yatra AND tatra` — passages that contain both words, in any order (the same
  as **All words** with `yatra tatra`);
* `dīrghaṃ jīvitam AND āyus` — each side of `AND` can be a phrase;
* `vāta OR vāyu` — passages that contain either.

`AND` and `OR` cannot be mixed in one search.

Outside Regex mode, `*` stands for any number of letters inside a word and
`?` for exactly one letter: `vāt*pitt` finds *vātapitta*, *vātapittakapha* …;
`?ṛta` finds *amṛta*, *ghṛta* … in the middle of words. Matching is by substring
anyway, so `yatra` already finds *yatra* inside longer words; `*` and `?` matter
mainly in the middle of a term, or with **whole words** ticked: then `yatra*`
finds words that begin with *yatra*, `*yatra` words that end with it, and
`*yatra*` words that contain it. A `*` never reaches across a space.

## Order of results

Results are not ranked by relevance. Texts are listed SARIT first, then GRETIL,
each alphabetically by title; under **More options → Order texts** you can
instead list the texts with the most matching passages first. Within each text
the passages are always in the order in which they occur in the text. **Passages
per text** limits how many are shown for each text (200 by default); the count
in each text's heading is always the full number.

## Ignoring things

* **diacritics** — `a` = `ā`, `s` = `ś` = `ṣ`, `n` = `ṅ ñ ṇ`, `m` = `ṃ`, `h` = `ḥ` …
  Useful when you are unsure of a spelling. In this mode `sh` also counts as `s`,
  so `shiva` and `vishnu` find *śiva* and *viṣṇu*.
* **word division** — spaces, hyphens, dots, `+`, `_` and avagraha are ignored, so
  `dīrghaṃjīvitam`, `dīrghaṃ jīvitam` and `dīrghaṃ-jīvitam` are the same. Many
  e-texts split words differently (GRETIL often uses `.` between words).
* **anusvāra / nasal** — a nasal before a consonant of its own class counts as
  anusvāra: `saṅkalpa` = `saṃkalpa`, `santi` = `saṃti`.

The options can be combined. **Whole words** requires the match to begin and end
at word boundaries (it cannot be combined with ignoring word division).
**Notes & variants** also searches editorial notes and apparatus readings; the
hits are labelled *note* or *variant*.

## References and citation

For every hit the site shows as much as the TEI markup provides:

* **Reference** (bold): the `xml:id`, `@n` numbering (e.g. `1.1.4` for
  parvan.adhyāya.verse) or `<label>` of the verse, sūtra or paragraph. For verses
  the reference of the half-verse or pāda is given when the file has one.
* **Division**: the chain of `<div>`s with their numbers and headings.
* **ed.**: page and line of the printed edition, from `<pb/>` and `<lb/>`
  (volume.page.line where the edition is so marked; the edition siglum is added
  when a file marks more than one edition).
* **XML l.** — a link to the exact line of the TEI file on GitHub.

**cite** copies a full citation to the clipboard, including the source edition
named in the TEI header and the file's GitHub address. **Download results**
saves all hits as a tab-separated file that opens in Excel or LibreOffice.
**context** opens the passage in the reader, where you can page through the
text, switch script and jump to a reference.

The search address in your browser's location bar records the query and all
options, so it can be bookmarked or cited.

## Speed

The first search over the whole corpus (about 240 million characters) downloads
the texts, which takes a little while; your browser then caches them and later
searches are much faster. Restricting the search to SARIT, GRETIL or a few
selected texts is quicker. **Stop** ends a search early and keeps the results
found so far.

## About the data

The site is rebuilt automatically whenever the repository changes: a script
(`website/_tools/extract.py`) reads every TEI file, splits it into passages at
the level of `<p>`, `<lg>`, `<ab>`, `<head>`, `<item>` etc., and records the
references described above. Notes, `<rdg>` readings and similar material are
kept apart from the main text. Where `<choice>` offers alternatives, the
corrected/regularised/expanded form is searched.

</div>
