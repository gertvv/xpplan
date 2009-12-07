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

class ThemeSnip {
	def add(form: NodeSeq) = {
		val theme = Theme.create.createdBy(User.currentUser)

		def checkAndSave(): Unit =
		theme.validate match {
			case Nil => theme.save ; S.notice("Added: \"" + theme.title + "\"")
			case xs => S.error(xs) ; S.mapSnippet("ThemeSnip.add", doBind)
		}

		def doBind(form: NodeSeq) =
		bind("theme", form,
			"title" -> theme.title.toForm,
			"description" -> theme.description.toForm,
			"value" -> theme.value.toForm,
			"submit" -> submit("New", checkAndSave))

		doBind(form)
	}

	def view(form: NodeSeq) = {
		val theme = currentTheme

		def doBind(form: NodeSeq) =
			bind("theme", form,
				"title" -> Text(theme.title.toString),
				"description" -> Text(theme.description.toString),
				"value" -> Text(theme.value.toString))

		doBind(form)
	}

	private def toShow =
		Theme.findAll(By(Theme.createdBy, User.currentUser),
			OrderBy(Theme.value, Descending))

	private def ajaxTitle(theme: Theme, reDraw: () => JsCmd) =
		swappable(<span>{theme.title}</span>,
			<span>{ajaxText(theme.title, v => {theme.title(v).save; reDraw()})}
			</span>)

	private def ajaxDescription(theme: Theme, reDraw: () => JsCmd) =
		swappable(<span>{theme.description}</span>,
			<span>{ajaxText(theme.description, v => {theme.description(v).save; reDraw()})}
			</span>)

	private def ajaxValue(theme: Theme, reDraw: () => JsCmd) =
		ajaxSelect(Theme.valueList, Full(theme.value.toString),
			v => {theme.value(v.toInt).save; reDraw()})

	private def doList(reDraw: () => JsCmd)(html: NodeSeq): NodeSeq =
		toShow.flatMap(theme => bind("theme", html,
			"value" -> ajaxValue(theme, reDraw),
			"title" -> ajaxTitle(theme, reDraw),
			"description" -> ajaxDescription(theme, reDraw),
			"link" -> link("/theme", () => CurrentThemeVar(theme), Text("View"))
		))

	object CurrentThemeVar extends RequestVar[Theme]({Theme.create.createdBy(User.currentUser)})

	def currentTheme = CurrentThemeVar.is

	def list (html: NodeSeq) = {
		val id = S.attr("themes_id").open_!

		def inner(): NodeSeq = {
			def reDraw() = SetHtml(id, inner())

			bind("theme", html,
				"list" -> doList(reDraw) _)
		}

		inner()
	}
}

// vim: set ts=4 sw=4 et:
