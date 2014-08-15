package com.benwaffle

object Browser extends App {
  println(Parser.parseHTML(
    "<html>" +
      "<body bg='#fff'>" +
        "<p>hi</p>" +
      "</body>" +
    "</html>"))
  println
  println(Parser.parseCSS(
    "h1, h2, h3 { margin: auto; color: #cc0000; }" +
    "div.note { margin-bottom: 20px; padding: 10px; }" +
    "#answer { display: none; }").mkString)
}
