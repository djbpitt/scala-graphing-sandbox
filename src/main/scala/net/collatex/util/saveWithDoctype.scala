package net.collatex.util

import scala.xml.*

@main def t(): Unit =
  val h = <html xmlns="http://www.w3.org/1999/xhtml">
    <head>
      <title>Hi</title>
    </head>
    <body>
      <h1>Hello</h1>
    </body>
  </html>
  val doctype = scala.xml.dtd.DocType("html")
  // Creates XML and doctype declarations correctly
  scala.xml.XML.save("test.xhtml", h, "UTF-8", true, doctype)


