package com.benwaffle

object Parser {
  def parseHTML(source: String) = {
    val nodes = new HTMLParser(0, source).parse_nodes
    if (nodes.length == 1) nodes.head
    else new Element("html", Map(), nodes)
  }

  def parseCSS(source:String) = new CSSParser(0, source).parse_rules
}
