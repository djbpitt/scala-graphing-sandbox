package net.collatex.reptilian

def createAlignmentBrowser(root: ExpandedNode, tokenArray: Vector[Token]) =
  val tableRows = createSingleColumnAlignmentTableRows(root, tokenArray)
  val flows = createSvgFlowModel(flattenNodeSeq(root), tokenArray)
  val css =
    """body {
      |  display: flex;
      |  flex-direction: row;
      |  height: 100vh;
      |}
      |body > div:first-child {
      |  flex: 1;
      |  overflow: auto;
      |}
      |body > div:nth-child(2) {
      |  flex: 3;
      |  overflow: auto;
      |}
      |table,
      |tr,
      |th,
      |td {
      |  border: 1px black solid;
      |  border-collapse: collapse;
      |}
      |th,
      |td {
      |  padding: 4px 3px 3px 3px;
      |}
      |ul {
      |  margin: -1px 0 0 0;
      |  padding-left: 1em;
      |  list-style-type: none;
      |  text-indent: -1em;
      |}
      |.sigla {
      |   font-size: small;
      |   font-weight: bold;
      |}
      |td:first-child {
      |  text-align: right;
      |  font-size: small;
      |  line-height: 1.33em;
      |}
      |tr {
      |  vertical-align: top;
      |}
      |.agreement {
      |  background-color: lightblue;
      |}
      |.agreementIndel {
      |  background-color: lightgoldenrodyellow;
      |}
      |.variation {
      |  background-color: bisque;
      |}
      |.variationIndel {
      |  background-color: thistle;
      |}
      |tr:first-child {
      |  background-color: lightgray;
      |}
      |tr.selected {
      |  outline: 3px red solid;
      |}
      |svg > g > g:hover {
      |  cursor: pointer;
      |}
      |g.selected {
      |  outline: 1.5px red solid;
      |}""".stripMargin
  val js = scala.xml.Unparsed("""//<![CDATA[
             |window.addEventListener("DOMContentLoaded", init);
             |function init() {
             |  let svgAlignmentPoints = document.querySelectorAll("svg > g > g");
             |  for (i = 0, len = svgAlignmentPoints.length; i < len; i++) {
             |    svgAlignmentPoints[i].addEventListener("click", centerTableRow);
             |  }
             |  let trAlignmentPoints = document.querySelectorAll("tr");
             |  for (i = 0, len = trAlignmentPoints.length; i < len; i++) {
             |    trAlignmentPoints[i].addEventListener("click", centerFlowG);
             |  }
             |}
             |function centerFlowG() {
             |  let sourceId = this.id;
             |  let targetId = sourceId.replace('t', 'v');
             |  clearHighlighting();
             |  let allTargets = document.querySelectorAll("svg > g > g");
             |  for (i = 0, len = allTargets.length; i < len; i++) {
             |    allTargets[i].classList.remove("selected");
             |  }
             |  document.getElementById(sourceId).classList.add("selected");
             |  document.getElementById(targetId).classList.add("selected");
             |  document.getElementById(targetId).scrollIntoView({
             |    block: "center", behavior: "smooth"
             |  });
             |}
             |function centerTableRow() {
             |  let sourceId = this.id;
             |  let targetId = sourceId.replace('v', 't');
             |  clearHighlighting();
             |  let allTargets = document.querySelectorAll("tr");
             |  for (i = 0, len = allTargets.length; i < len; i++) {
             |    allTargets[i].classList.remove("selected");
             |  }
             |  document.getElementById(sourceId).classList.add("selected");
             |  document.getElementById(targetId).classList.add("selected");
             |  document.getElementById(targetId).scrollIntoView({
             |    block: "center", behavior: "smooth"
             |  });
             |}
             |function clearHighlighting() {
             |  let allSources = document.querySelectorAll("svg > g > g");
             |  for (i = 0, len = allSources.length; i < len; i++) {
             |    allSources[i].classList.remove("selected");
             |  }
             |}
             |//]]>""".stripMargin)
  val htmlHead =
    <head>
      <title>Alignment browser</title>
      <style>{css}</style>
      <script type="text/javascript">{js}</script>
    </head>
  val result =
    <html xmlns="http://www.w3.org/1999/xhtml">
      {htmlHead}
      <body>
        <div>{flows}</div>
        <div>{tableRows}</div>
      </body>
    </html>
  result
