package com.benwaffle

class Node(children: List[Node], data: Any)
class Text(children: List[Node], data: String) extends Node(children, data) {
  def this(text: String) =
    this(List(), text)
  override def toString = '"' + data + '"'
}
class Element(children: List[Node], data: ElementData) extends Node(children, data) {
  def this(name: String, attrs: Map[String, String], children: List[Node]) =
    this(children, new ElementData(name, attrs))
  override def toString = data + "[" + children + "]"
}
class ElementData(tag_name: String, attributes: Map[String, String]) {
  override def toString = "<" + tag_name + " " + (if (attributes.nonEmpty) attributes) + ">\n"
}

object Browser {
  def main(args: Array[String]) {
    println(Parser.parse(
      "<html>" +
        "<body bg='#fff'>" +
          "<p>hi</p>" +
        "</body>" +
      "</html>"))
  }
}
