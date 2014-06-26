<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:java="http://xml.apache.org/xalan/java" exclude-result-prefixes="java">
<xsl:output encoding="utf-8" indent="yes" method="xml" version="1.0"/>
<!-- by andrew ollett, using transliteration
  routines from somadeva vasudeva. last update:
  june. 24, 2014. !-->
<!-- THERE ARE TWO VERSIONS OF THIS TRANSFORMATION.
  This one presumes that avagrahas are input as +a,
  leaving single quotes to mark quotations. 
  !-->

<xsl:param name="Transliterate">
  OM > ॐ;

  :: [:Latin:] lower ();

  $LTR = [{a}{ā}{i}{ī}{u}{ū}{ṛ}{ṝ}{ḷ}{ḹ}{e}{ai}{o}{au}{ḥ}{ṃ}{ṁ}{k}{kh}{g}{gh}{ṅ}{c}{ch}{j}{jh}{ñ}{ṭ}{ṭh}{ḍ}{ḍh}{ṇ}{t}{th}{d}{dh}{n}{p}{ph}{b}{bh}{m}{y}{r}{l}{v}{ś}{ṣ}{s}{h}];
  $nonletter = [:^letter:];
  $rMedVowel = [{a}{ā}{i}{ī}{u}{ū}{ṛ}{ṝ}{ḷ}{ḹ}{e}{ai}{o}{au}];
  $dMedVowel = [अआइईउऊऋॠएऐओऔऌ];
  $anyMedVowel = [[$rMedVowel][$dMedVowel]];
  $rCons = [{k}{kh}{g}{gh}{ṅ}{c}{ch}{j}{jh}{ñ}{ṭ}{ṭh}{ḍ}{ḍh}{ṇ}{t}{th}{d}{dh}{n}{p}{ph}{b}{bh}{m}{y}{r}{l}{v}{ś}{ṣ}{s}{h}];
  $rVoiced = [{g}{gh}{j}{jh}{ḍ}{ḍh}{ṇ}{d}{dh}{n}{b}{bh}{m}{y}{r}{l}{v}{h}];
  $dCons = [कखगघङचछजझञटठडढणतथदधनपफबभमयरलवशषसह];
  $anyCons = [[$rCons][$dCons]];

  'nn ' } $rMedVowel > 'nn';
  'm ' } $rMedVowel > 'm';
  'v ' } $rMedVowel > 'v';
  'y ' } $rMedVowel > 'y';
  'r ' } $rMedVowel > 'r';
  'r ' } $rVoiced > 'r';
  'ś c' > 'śc';
  's t' > 'st';
  'c c' > 'cc';
  '+a' > 'ऽ';
  ::Null;

  $nonletter { ā > आ;
  $nonletter { i > इ;
  $nonletter { ī > ई;
  $nonletter { u > उ;
  $nonletter { ū > ऊ;
  $nonletter { ṛ > ऋ;
  $nonletter { ṝ > ॠ;
  $nonletter { e > ए;
  $nonletter { ĕ > ऎ;
  $nonletter { ai > ऐ;
  $nonletter { o > ओ;
  $nonletter { ŏ > ऒ;
  $nonletter { au > औ;
  $nonletter { a > अ;

  $anyMedVowel { ā > आ;
  $anyMedVowel { i > इ;
  $anyMedVowel { ī > ई;
  $anyMedVowel { u > उ;
  $anyMedVowel { ū > ऊ;
  $anyMedVowel { ṛ > ऋ;
  $anyMedVowel { ṝ > ॠ;
  $anyMedVowel { e > ए;
  $anyMedVowel { ĕ > ऎ;
  $anyMedVowel { o > ओ;
  $anyMedVowel { ŏ > ऒ;
  $anyMedVowel { ï > इ;
  $anyMedVowel { ü > ऊ;
  $anyMedVowel { a > अ;

  k } $rMedVowel > क;
  kh } $rMedVowel > ख;
  g } $rMedVowel > ग;
  gh } $rMedVowel > घ;
  ṅ } $rMedVowel > ङ;
  c } $rMedVowel > च;
  ch } $rMedVowel > छ;
  j } $rMedVowel > ज;
  jh } $rMedVowel > झ;
  ñ } $rMedVowel > ञ;
  ṭ } $rMedVowel > ट;
  ṭh } $rMedVowel > ठ;
  ḍ } $rMedVowel > ड;
  ḍh } $rMedVowel > ढ;
  ṇ } $rMedVowel > ण;
  t } $rMedVowel > त;
  th } $rMedVowel > थ;
  d } $rMedVowel > द;
  dh } $rMedVowel > ध;
  n } $rMedVowel > न;
  p } $rMedVowel > प;
  ph } $rMedVowel > फ;
  b } $rMedVowel > ब;
  bh } $rMedVowel > भ;
  m } $rMedVowel > म;
  y } $rMedVowel > य;
  r } $rMedVowel > र;
  l } $rMedVowel > ल;
  v } $rMedVowel > व;
  ś } $rMedVowel > श;
  ṣ } $rMedVowel > ष;
  s } $rMedVowel > स;
  h } $rMedVowel > ह;

  ā > 'ा' ;
  i > 'ि' ;
  ī > 'ी' ;
  u > 'ु' ;
  ū > 'ू' ;
  ṛ > 'ृ' ;
  ṝ > 'ॄ' ;
  ḷ > 'ॢ' ;
  ḹ > 'ॣ' ;
  e > 'े' ;
  ĕ > 'ॆ';
  ai > 'ै' ;
  o > 'ो' ;
  ŏ > 'ॊ';
  au > 'ौ' ;
  ṃ > 'ं' ;
  ṁ > 'ँ' ;
  ḥ > 'ः' ;
  aḥ > 'ः' ; 
  aṃ > 'ं' ;
  aṁ > 'ँ' ;

  $anyCons { a > ;

  kh > ख्;
  k > क्;
  gh > घ्;
  g > ग्;
  ṅ > ङ्;
  ch > छ्;
  c > च्;
  jh > झ्;
  j > ज्;
  ñ > ञ्;
  ṭh > ठ्;
  ṭ > ट्;
  ḍh > ढ्;
  ḍ > ड्;
  ṇ > ण्;
  th > थ्;
  t > त्;
  dh > ध्;
  d > द्;
  n > न्;
  ph > फ्;
  p > प्;
  bh > भ्;
  b > ब्;
  m > म्;
  y > य्;
  r > र्;
  l > ल्;
  v > व्;
  ś > श्;
  ṣ > ष्;
  s > स्;
  h > ह्;

  ï > इ;
  ü > उ; 

	0 > ० ;
	1 > १ ;
	2 > २ ;
	3 > ३ ;
    4 > ४ ;
    5 > ५ ;  
    6 > ६ ;
    7 > ७ ;
	8 > ८ ;
	9 > ९ ; 

  #$single = \' ;
  #$space = ' ' ;
  
  #$space $single > $single;
  #::Null;

  #$single > ऽ;

	'||' > ॥ ;
	'|' > ।;
  '//' > ॥;
  '/' > ।;
  '.' > '॰';

    'म् ' }  अ > 'म';
  'म् ' } आ > 'मा';
  'म् ' } इ > 'मि';
  'म् ' } ई > मी;
  'म् ' } उ > मु;
  'म् ' } ऊ > मू;
  'म् ' } ऋ > मृ;
  'म् ' } ॠ > मॄ;
  'म् ' } ऌ > मॢ;
  'म् ' } ए > मे;
  'म् ' } ऐ > मै;
  'म् ' } ओ > मो;
  'म् ' } औ > मै; 

</xsl:param>

<xsl:variable name="transliterator" select="java:com.ibm.icu.text.Transliterator.createFromRules('',$Transliterate,Transliterator.FORWARD)"/>

<xsl:template match="@*|node()">
  <xsl:copy>
    <xsl:apply-templates select="@*|node()"/>
  </xsl:copy>
</xsl:template>

<!-- changes the xml:lang attribute !-->
<xsl:template match="@xml:lang">
  <xsl:attribute name="xml:lang">
    <xsl:choose>
      <xsl:when test=". = 'sa-Latn'">
        <xsl:text>sa-Deva</xsl:text>
      </xsl:when>
      <xsl:otherwise><xsl:value-of select="."/></xsl:otherwise>
    </xsl:choose>
  </xsl:attribute>
</xsl:template>

<!-- changes all devanagari text to romanization, provided
  (1) that the top-level text element has the attribute
        xml:lang="sa-Deva"; and
  (2) that the text doesn't belong to any element that
        has the xml:lang="en" attribute. !-->
<xsl:template match="text[@xml:lang='sa-Latn']//text()">
  <xsl:choose>
    <xsl:when test="ancestor::*[@xml:lang='en']">
      <xsl:copy>
        <xsl:apply-templates/>
      </xsl:copy>
    </xsl:when>
    <xsl:otherwise>
      <xsl:variable name="output" select="java:transliterate($transliterator, .)"/>
      <xsl:value-of select="$output"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<!-- this changes the xml:id of the top-level TEI
  element from ending in '-roman' to ending in '-dn',
  which i use for switching between the files. you
  can ignore it if you don't use this element. !-->
<!-- by the way, this is necessary only because 
  i'm using an XSL 1.0 processor, which can't access
  the base uri. !-->
<xsl:template match="@xml:id[parent::TEI]">
  <xsl:variable name="filename" select="."/>
  <xsl:attribute name="xml:id">
    <xsl:variable name="newfilename" select="substring-before($filename, '-')"/> <!-- find way to select last instance !-->
    <xsl:value-of select="concat($newfilename, '-dn')"/>
  </xsl:attribute>
</xsl:template>

<!-- this adds a change to the log about the transliteration !-->
<xsl:template match="revisionDesc">
  <xsl:copy>
    <change when="2014-06-24" who="#aso"> <!-- CHANGE: XSL 1.0 doesn't support current-Datetime() !-->
      Automatic conversion from Roman to Devanāgarī.
    </change>
    <xsl:apply-templates select="@* | node()"/>
  </xsl:copy>
</xsl:template>

</xsl:stylesheet>
