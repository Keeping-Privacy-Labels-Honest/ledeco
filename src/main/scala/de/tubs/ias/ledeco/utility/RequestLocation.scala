package de.tubs.ias.ledeco.utility

object RequestLocation extends Enumeration {
  type Location = Value
  val URL, Cookie, Body, Mixed, Header = Value
}
