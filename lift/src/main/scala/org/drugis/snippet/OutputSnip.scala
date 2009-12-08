package org.drugis.snippet

import org.drugis._
import model._

import net.liftweb._
import net.liftweb.common._
import http._
import SHtml._
import S._

import js._
import JsCmds._

import mapper._

import util._
import Helpers._

import scala.xml.{NodeSeq, Text}

class OutputSnip {
	def view(html: NodeSeq) = {
		bind("output", html,
			"stories" -> textarea("stories here", v=>{}),
			"themes" -> textarea("themes here", v=>{})
		)
	}
}

// vim: set ts=4 sw=4 et:
