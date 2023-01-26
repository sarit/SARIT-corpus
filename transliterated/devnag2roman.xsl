<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:java="http://xml.apache.org/xalan/java" exclude-result-prefixes="java">
<xsl:output encoding="utf-8" indent="no" method="xml" version="1.0"/>
<!-- by andrew ollett, using transliteration
  routines from somadeva vasudeva. last update:
  june. 24, 2014. !-->

<xsl:param name="Transliterate">
  <!-- :: [:Latin:] lower (); !-->

  $LTR = [{a}{ā}{i}{ī}{u}{ū}{ṛ}{ṝ}{ḷ}{ḹ}{e}{ai}{o}{au}{ḥ}{ṃ}{ṁ}{k}{kh}{g}{gh}{ṅ}{c}{ch}{j}{jh}{ñ}{ṭ}{ṭh}{ḍ}{ḍh}{ṇ}{t}{th}{d}{dh}{n}{p}{ph}{b}{bh}{m}{y}{r}{l}{ḷ}{v}{ś}{ṣ}{s}{h}];
  $nonletter = [:^letter:];
  $rMedVowel = [{a}{ā}{i}{ī}{u}{ū}{ṛ}{ṝ}{ḷ}{ḹ}{e}{ai}{o}{au}];
  $dMedVowel = [आइईउऊऋॠएऐओऔऌ];
  $shortA = [अ];
  $dCons = [कखगघङचछजझञटठडढणतथदधनपफबभमयरलळवशषसह];
  $dInherent = [ािीुूृॄेैॆॅोौॉॊॢॣ];
  $virama = '्';
  $dI = [[$dInherent][$virama]];
  $anyMedVowel = [[$rMedVowel][$dMedVowel]];
  $rCons = [{k}{kh}{g}{gh}{ṅ}{c}{ch}{j}{jh}{ñ}{ṭ}{ṭh}{ḍ}{ḍh}{ṇ}{t}{th}{d}{dh}{n}{p}{ph}{b}{bh}{m}{y}{r}{l}{ḷ}{v}{ś}{ṣ}{s}{h}];
  $rVoiced = [{g}{gh}{j}{jh}{ḍ}{ḍh}{ṇ}{d}{dh}{n}{b}{bh}{m}{y}{r}{l}{ḷ}{v}{h}];
  $anyDLetter = [[$dMedVowel][$dCons][$dInherent][$shortA]];
  $dVowels = [[$dMedVowel][$dInherent]];
  $nonDLetter = [^$anyDLetter];
  $anyCons = [[$rCons][$dCons]];

  अ { इ > ï;
  अ { उ > ü;

  ::Null;

<!-- initial vowels !-->
  $nonDLetter { आ > ā;
  $nonDLetter { इ > i;
  $nonDLetter { ई > ī;
  $nonDLetter { उ > u;
  $nonDLetter { ऊ > ū;
  $nonDLetter { ऋ > ṛ;
  $nonDLetter { ॠ > ṝ;
  $nonDLetter { ए > e;
  $nonDLetter { ऎ > ĕ;
  $nonDLetter { ऍ > ĕ;
<!-- we'll have to figure out a way to distinguish ऍ and ऎ !-->
  $nonDLetter { ऐ > ai;
  $nonDLetter { ओ > o;
  $nonDLetter { ऒ > ŏ;
  $nonDLetter { ऑ > ŏ;
<!-- ditto ऑ and ऒ !-->
  $nonDLetter { औ > au;
  $nonDLetter { अ > a;

  ::Null;

  $dVowels { इ > i;
  $dVowels { उ > u;

  ::Null;

<!-- medial vowels followed by vowels !-->
  आ > ā;
  इ > ï;
  ई > ī;
  उ > ü;
  ऊ > ū;
  ऋ > ṛ;
  ॠ > ṝ;
  ए > e;
  ऎ > ĕ;
  ऍ > ĕ;
  ऐ > ai;
  ओ > o;
  ऒ > ŏ;
  ऑ > ŏ;
  औ > au;
  अ > a;

  ::Null;

<!-- consonants !-->
  क } $dI > k;
  ख } $dI > kh;
  ग } $dI > g;
  घ } $dI > gh;
  ङ } $dI > ṅ;
  च } $dI > c;
  छ } $dI > ch;
  ज } $dI > j;
  झ } $dI > jh;
  ञ } $dI > ñ;
  ट } $dI > ṭ;
  ठ } $dI > ṭh;
  ड } $dI > ḍ;
  ढ } $dI > ̣dh;
  ण } $dI > ṇ;
  त } $dI > t;
  थ } $dI > th;
  द } $dI > d;
  ध } $dI > dh;
  न } $dI > n;
  प } $dI > p;
  फ } $dI > ph;
  ब } $dI > b;
  भ } $dI > bh;
  म } $dI > m;
  य } $dI > y;
  र } $dI > r;
  ल } $dI > l;
  ळ } $dI > ḷ;
  व } $dI > v;
  श } $dI > ś;
  ष } $dI > ṣ;
  स } $dI > s;
  ह } $dI > h;

  $virama > ;

<!-- inherent vowels !-->
  'ा' > ā;
  'ि' > i;
  'ी' > ī;
  'ु' > u;
  'ू' > ū;
  'ृ' > ṛ;
  'ॄ' > ṝ;
  'ॢ' > ḷ;
  'ॣ' > ḹ;
  'े' > e;
  'ॆ' > ĕ;
  'ॅ' > ĕ;
  'ै' > ai;
  'ो' > o;
  'ॊ' > ŏ;
  'ॉ' > ŏ;
  'ौ' > au;
  'ं' > ṃ;
  'ँ' > ṁ;
  'ः' > ḥ;

  ::Null;

  क > ka;
  ख > kha;
  ग  > ga;
  घ  > gha;
  ङ > ṅa;
  च > ca;
  छ > cha;
  ज > ja;
  झ > jha;
  ञ > ña;
  ट > ṭa;
  ठ > ṭha;
  ड > ḍa;
  ढ > ḍha;
  ण > ṇa;
  त > ta;
  थ > tha;
  द > da;
  ध > dha;
  न > na;
  प > pa;
  फ > pha;
  ब > ba;
  भ > bha;
  म > ma;
  य > ya;
  र > ra;
  ल > la;
  ळ > ḷa;
  व > va;
  श > śa;
  ष > ṣa;
  स > sa;
  ह > ha;

  ० > 0;
  १ > 1;
  २ > 2;
  ३ > 3;
  ४ > 4;
  ५ > 5;
  ६ > 6;
  ७ > 7;
  ८ > 8;
  ९ > 9;
  ॰ > '.';

  ॥ > '||';
  । > '|';

  $single = \' ;
  $space = ' ' ;
  ऽ > $space $single; 

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
      <xsl:when test=". = 'sa-Deva'">
        <xsl:text>sa-Latn</xsl:text>
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
<xsl:template match="text[@xml:lang='sa-Deva']//text()">
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
  element from ending in '-dn' to ending in '-roman',
  which i use for switching between the files. you
  can ignore it if you don't use this element. !-->
<!-- by the way, this is necessary only because 
  i'm using an XSL 1.0 processor, which can't access
  the base uri. !-->
<xsl:template match="@xml:id[parent::TEI]">
  <xsl:variable name="filename" select="."/>
  <xsl:attribute name="xml:id">
    <xsl:variable name="newfilename">
      <xsl:call-template name="substring-before-last">
        <xsl:with-param name="list" select="normalize-space(.)"/>
        <xsl:with-param name="delimiter" select="'-'"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:value-of select="concat($newfilename, '-roman')"/>
  </xsl:attribute>
</xsl:template>

<xsl:template name="substring-before-last">
<!--passed template parameter -->
  <xsl:param name="list"/>
  <xsl:param name="delimiter"/>
  <xsl:choose>
    <xsl:when test="contains($list, $delimiter)">
    <!-- get everything in front of the first delimiter -->
      <xsl:value-of select="substring-before($list,$delimiter)"/>
      <xsl:choose>
        <xsl:when test="contains(substring-after($list,$delimiter),$delimiter)">
          <xsl:value-of select="$delimiter"/>
        </xsl:when>
      </xsl:choose>
      <xsl:call-template name="substring-before-last">
      <!-- store anything left in another variable -->
        <xsl:with-param name="list" select="substring-after($list,$delimiter)"/>
        <xsl:with-param name="delimiter" select="$delimiter"/>
      </xsl:call-template>
    </xsl:when>
  </xsl:choose>
</xsl:template>

<!-- this adds a change to the log about the transliteration !-->
<xsl:template match="revisionDesc">
  <xsl:copy>
    <change when="2014-06-24" who="#aso"> <!-- CHANGE: XSL 1.0 doesn't support current-Datetime() !-->
      Automatic conversion from Devanāgarī to Roman.
    </change>
    <xsl:apply-templates select="@* | node()"/>
  </xsl:copy>
</xsl:template>

</xsl:stylesheet>
