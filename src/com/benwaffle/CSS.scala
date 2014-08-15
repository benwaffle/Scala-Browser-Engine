package com.benwaffle

import scala.util.control.Breaks._

class CSSParser(var pos: Int, var input: String) {
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

  def parse_simple_selector = {
    val result = new SimpleSelector(None, None, List())
    breakable {
      while (!eof)
        next_char match {
          case '#' => {
            consume_char
            result.id = Some(parse_identifier)
          }
          case '.' => {
            consume_char
            result.clazz ::= parse_identifier
          }
          case '*' => consume_char
          case c =>
            if (valid_identifier_char(c)) result.tag_name = Some(parse_identifier)
            else break
        }
    }
    result
  }
  def parse_identifier = consume_while(valid_identifier_char)
  def valid_identifier_char(c: Char) =
    ('a' until 'z' contains c) ||
    ('A' until 'Z' contains c) ||
    ('0' until '9' contains c) ||
    c == '-' || c == '_'

  def parse_rules = {
    var rules = List[Rule]()
    breakable { while(true) {
      consume_whitespace
      if (eof) break
      rules ::= parse_rule
    } }
    rules
  }
  def parse_rule = new Rule(parse_selectors, parse_declarations)
  def parse_selectors = {
    var selectors = List[Selector]()
    breakable { while (true) {
      selectors ::= parse_simple_selector
      consume_whitespace
      next_char match {
        case ',' => consume_char
        case '{' => break
        case c => {
          printf("Error: unexpected char %c in selector list", c)
          System.exit(1)
        }
      }
      consume_whitespace
    } }
    selectors.sortBy(_.specificity)
  }
  def parse_declarations = {
    assert(consume_char == '{')
    var declarations = List[Declaration]()
    breakable { while(true) {
      consume_whitespace
      if (next_char == '}'){
        consume_char
        break
      }
      declarations ::= parse_declaration
    } }
    declarations
  }
  def parse_declaration = {
    val property_name = parse_identifier
    consume_whitespace
    assert(consume_char == ':')
    consume_whitespace
    val value = parse_value
    consume_whitespace
    assert(consume_char == ';')
    new Declaration(property_name, value)
  }
  def parse_value: Value = {
    next_char match {
      case it if '0' until '9' contains it => parse_length
      case '#' => parse_color
      case _ => new Keyword(parse_identifier)
    }
  }
  def parse_length = new Length(parse_float, parse_unit)
  def parse_float = consume_while(c => ('0' <= c && c <= '9') || c == '.').toFloat
  def parse_unit: DistUnit = parse_identifier.toLowerCase match {
    case "px" => new Px
    case _ => {
      println("unrecognized unit")
      new DistUnit
    }
  }
  def parse_color = {
    assert(consume_char == '#')
    new Color(parse_hex_pair, parse_hex_pair, parse_hex_pair, 255)
  }
  def parse_hex_pair = {
    val str = Integer.parseInt(input.substring(pos,pos+2),16)
    pos += 2
    str
  }
}

class Stylesheet(var rules: List[Rule])

class Rule(var selectors: List[Selector], var declarations: List[Declaration]) {
  override def toString = selectors.mkString(",") + "{\n" + declarations.mkString("\n") + "\n}\n"
}

class Selector {
  type Specificity = (Int, Int, Int)
  def specificity: Specificity = this match {
    case ss: SimpleSelector =>
      (ss.tag_name.getOrElse("").length,
      ss.clazz.length,
      ss.tag_name.getOrElse("").length)
    case _ => (0,0,0)
  }
}
class SimpleSelector(var tag_name: Option[String], var id: Option[String], var clazz: List[String]) extends Selector {
  override def toString =
    if (tag_name.isDefined) tag_name.get
    else if (id.isDefined) "#" + id.get
    else clazz.map("." + _).mkString(",")
}

class Declaration(name: String, value: Value) {
  override def toString = name + ": " + value + ";"
}

class Value
class Keyword(keyword: String) extends Value {
  override def toString = keyword
}
class Color(r: Int, g: Int, b: Int, a: Int) extends Value {
  override def toString =
    "#" + Integer.toHexString(r) + Integer.toHexString(g) + Integer.toHexString(b) + Integer.toHexString(a)
}
class Length(length: Float, units: DistUnit) extends Value {
  override def toString = length + units.toString
}

class DistUnit
class Px extends DistUnit {
  override def toString = "px"
}