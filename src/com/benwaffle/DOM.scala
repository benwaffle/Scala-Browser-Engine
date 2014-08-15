package com.benwaffle

import scala.util.control.Breaks._

class HTMLParser(var pos: Int, var input: String) {
  def next_char = input.charAt(pos)
  def starts_with(s: String) = input.substring(pos).startsWith(s)
  def eof = pos >= input.length()

  def consume_char = {
    pos += 1
    input.charAt(pos - 1)
  }
  def consume_while(test: Char => Boolean) = {
    var ret = ""
    while (!eof && test(next_char))
      ret += consume_char
    ret
  }
  def consume_whitespace = consume_while(_.isWhitespace)

  def parse_tag_name = consume_while(_.isLetterOrDigit)
  def parse_node: Node = next_char match {
    case '<' => parse_element
    case _ => parse_text
  }
  def parse_text = new Text(consume_while(_ != '<'))
  def parse_element = {
    assert(consume_char == '<')
    val tag_name = parse_tag_name
    val attrs = parse_attributes
    assert(consume_char == '>')

    val children = parse_nodes
    assert(consume_char == '<')
    assert(consume_char == '/')
    assert(parse_tag_name == tag_name)
    assert(consume_char == '>')
    new Element(tag_name, attrs, children)
  }

  def parse_attr = {
    val name = parse_tag_name
    assert(consume_char == '=')
    val value = parse_attr_value
    Map(name -> value)
  }
  def parse_attr_value = {
    val open_quote = consume_char
    assert(open_quote == '"' || open_quote == '\'')
    val value = consume_while(_ != open_quote)
    assert(consume_char == open_quote)
    value
  }
  def parse_attributes = {
    var attrs = Map[String, String]()
    breakable { while (true) {
      consume_whitespace
      if (next_char == '>') break
      attrs ++= parse_attr
    } }
    attrs
  }

  def parse_nodes = {
    var nodes = List[Node]()
    breakable { while (true) {
      consume_whitespace
      if (eof || starts_with("</")) break
      nodes ::= parse_node
    } }
    nodes
  }
}

class Node(children: List[Node], data: Any);
class Text(children: List[Node], data: String) extends Node(children, data) {
  def this(text: String) =
    this(List(), text)
  override def toString = '"' + data + '"'
}
class Element(children: List[Node], data: ElementData) extends Node(children, data) {
  def this(name: String, attrs: Map[String, String], children: List[Node]) =
    this(children, new ElementData(name, attrs))
  override def toString = data + "\n" + children.mkString + "\n" + data.toStringClose
}
class ElementData(tag_name: String, attributes: Map[String, String]) {
  override def toString = "<" + tag_name + " " + (if (attributes.nonEmpty) attributes.mkString) + ">"
  def toStringClose = "</" + tag_name + ">"
}