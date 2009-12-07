package org.drugis.snippet

import scala.xml.{NodeSeq}
import org.drugis._
import model._

class Util {
	def in(html: NodeSeq) =
		if (User.loggedIn_?) html else NodeSeq.Empty
	def out (html: NodeSeq) =
		if (User.loggedIn_?) NodeSeq.Empty else html
}

// vim: set ts=4 sw=4 et:
