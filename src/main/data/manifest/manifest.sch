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
      <sch:let name="my-siglum" value="."/>
      <sch:let name="preceding-same-sigla"
        value="../preceding-sibling::cx:witness[@siglum eq $my-siglum]"/>
      <sch:let name="following-same-sigla-pos"
        value="../following-sibling::cx:witness[@siglum eq $my-siglum] ! (count(preceding-sibling::cx:witness) + 1)"/>
      <sch:report test="empty($preceding-same-sigla) and count($following-same-sigla-pos) eq 1"> The
        current siglum is duplicated in the witness at offset <sch:value-of
          select="$following-same-sigla-pos"/>. </sch:report>
      <sch:report test="empty($preceding-same-sigla) and count($following-same-sigla-pos) gt 1"> The
        current siglum is duplicated in the witnesses at offsets <sch:value-of
          select="string-join($following-same-sigla-pos, ', ')"/>. </sch:report>
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
        test="empty($preceding-resolved-same-uris) and count($following-resolved-same-uris-pos) eq 1"
        > The current @url (<sch:value-of select="."/>) is duplicated in the witness at offset
          <sch:value-of select="$following-resolved-same-uris-pos"/>. </sch:report>

      <sch:report
        test="empty($preceding-resolved-same-uris) and count($following-resolved-same-uris-pos) gt 1"
        > The current @url (<sch:value-of select="."/>) is duplicated in the witnesses at offsets
          <sch:value-of select="string-join($following-resolved-same-uris-pos, ', ')"/>. </sch:report>

      <!-- ============================================================== -->
      <!-- Verify that url can be reached.                                -->
      <!-- ============================================================== -->
      <sch:assert test="unparsed-text-available($my-resolved-uri)"> The url <sch:value-of select="."
        /> cannot be resolved. </sch:assert>

    </sch:rule>
  </sch:pattern>

</sch:schema>
