<?xml version="1.0" encoding="UTF-8"?>
<sch:schema xmlns:sch="http://purl.oclc.org/dsdl/schematron" queryBinding="xslt3"
  xmlns:cx="http://interedition.eu/collatex/ns/1.0"
  xmlns:sqf="http://www.schematron-quickfix.com/validator/process">
  <sch:ns prefix="cx" uri="http://interedition.eu/collatex/ns/1.0"/>
  <sch:title>Validate CollateX Reptilian witness manifest</sch:title>
  <sch:pattern>
    <sch:rule context="cx:witness/@siglum">
      <!-- ============================================================== -->
      <!-- Report duplicate sigla                                         -->
      <!-- Highlights first (only) of duplicates, reports offsets of      -->
      <!--   others                                                       -->
      <!-- ============================================================== -->
      <!-- Current siglum and offset of its <witenss> parent              -->
      <!-- ============================================================== -->
      <sch:let name="my-siglum" value="."/>
      <sch:let name="my-pos" value="count(../preceding-sibling::cx:witness) + 1"/>
      <!-- ============================================================== -->
      <!-- Matching preceding sigla and offsets of their <witness> parents-->
      <!-- ============================================================== -->
      <sch:let name="preceding-same-sigla"
        value="../preceding-sibling::cx:witness[@siglum eq $my-siglum]"/>
      <sch:let name="preceding-same-sigla-pos"
        value="$preceding-same-sigla ! count(./preceding-sibling::cx:witness) + 1"/>
      <!-- ============================================================== -->
      <!-- Matching following sigla and offsets of their <witness> parents-->
      <!-- ============================================================== -->
      <sch:let name="following-same-sigla"
        value="../following-sibling::cx:witness[@siglum eq $my-siglum]"/>
      <sch:let name="following-same-sigla-pos"
        value="$following-same-sigla ! (count(preceding-sibling::cx:witness) + 1)"/>
      <!-- ============================================================== -->
      <!-- Report duplicate sigla                                         -->
      <!-- ============================================================== -->
      <sch:report test="empty($preceding-same-sigla) and count($following-same-sigla-pos) ge 1"> The
        siglum <sch:value-of select="$my-siglum"/> is duplicated at witness offsets: <sch:value-of
          select="string-join(sort(($my-pos, $preceding-same-sigla-pos, $following-same-sigla-pos)), ', ')"
        />. </sch:report>
    </sch:rule>

    <sch:rule context="cx:witness/@url">
      <!-- ============================================================== -->
      <!-- Report duplicate URLs                                          -->
      <!-- Highlights first (only) of duplicates, reports offsets of      -->
      <!--   others                                                       -->
      <!-- Resolves URIs (i.e., recognizes when relative and absolute are -->
      <!--   the same)                                                    -->
      <!-- Currently ignores fragment identifiers and query strings       -->
      <!-- ============================================================== -->
      <sch:let name="my-base-uri" value="string((document-uri(/), base-uri(/))[. ne ''][1])"/>
      <sch:let name="my-resolved-uri" value="resolve-uri(., $my-base-uri)"/>
      <sch:let name="preceding-resolved-same-uris" value="
          (../preceding-sibling::cx:witness/@url
          ! resolve-uri(., $my-base-uri))[. eq $my-resolved-uri]"/>
      <sch:let name="following-resolved-same-uris-pos" value="
          ../following-sibling::cx:witness[
          resolve-uri(@url, $my-base-uri) eq $my-resolved-uri
          ] ! (count(preceding-sibling::cx:witness) + 1)"/>
      <sch:report
        test="empty($preceding-resolved-same-uris) and count($following-resolved-same-uris-pos) ge 1"
        > The current @url (<sch:value-of select="."/>) is duplicated at witness offsets
          <sch:value-of select="string-join($following-resolved-same-uris-pos, ', ')"/>. </sch:report>
      <!-- ============================================================== -->
      <!-- Verify that url can be reached.                                -->
      <!-- ============================================================== -->
      <sch:assert test="unparsed-text-available($my-resolved-uri)"> The url <sch:value-of select="."
        /> (associated with siglum <sch:value-of select="../@siglum"/> at witness offset
          <sch:value-of select="count(../preceding-sibling::cx:witness) + 1"/>) cannot be resolved.
      </sch:assert>
    </sch:rule>
  </sch:pattern>

</sch:schema>
