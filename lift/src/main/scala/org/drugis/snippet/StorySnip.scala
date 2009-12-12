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

class StorySnip {
	def view(form: NodeSeq) = {
		val story = currentStory
		val id = S.attr("view_id").open_!

		def inner(): NodeSeq = {
			def reDraw() = SetHtml(id, inner())

			bind("story", form,
				"done" -> ajaxCheckbox(story.done, v => {story.done(v).save; reDraw()}),
				"ready" -> ajaxCheckbox(story.ready, v => {story.ready(v).save; reDraw()}),
				"value" -> ajaxSelect(Story.valueList, Full(story.value.toString),
					v => {story.value(v.toInt).save; reDraw()}),
				"complexity" -> ajaxSelect(Story.complexityList, Full(story.complexity.toString),
					v => {story.complexity(v.toInt).save; reDraw()}),
				"title" -> title(story, reDraw),
				"description" -> description(story, reDraw),
				"link" -> link("/story", () => CurrentStoryVar(story), Text("View")),
				"hasDeps" -> conditionalDeps(story, reDraw) _,
				"addDep" -> ajaxSelectObj(storyList, Empty, (dep:Story)=>{addDep(story, dep); reDraw()}),
				"hasThemes" -> conditionalThemes(story, reDraw) _,
				"addTheme" -> ajaxSelectObj(themesList, Empty, (theme:Theme)=>{addTheme(story, theme); reDraw()})
				//"stories" -> storiesList(theme, reDraw)_)
			)
		}

		inner()
	}

	object CurrentStoryVar extends RequestVar[Story]({Story.create.createdBy(User.currentUser)})

	def currentStory = CurrentStoryVar.is

	def add(form: NodeSeq) = {
		val story = Story.create.createdBy(User.currentUser)

		def checkAndSave(): Unit =
		story.validate match {
			case Nil => story.save ; S.notice("Added: \"" + story.title + "\"")
			case xs => S.error(xs) ; S.mapSnippet("StorySnip.add", doBind)
		}

		def doBind(form: NodeSeq) =
		bind("story", form,
			"title" -> story.title.toForm,
			"description" -> textarea(story.description.toString,
					(v) => {story.description(v)}),
			"submit" -> submit("New", checkAndSave))

		doBind(form)
	}

	private def toShow =
		Story.findAll(
			if (QueryNotDone) By(Story.done, false)
			else Ignore[Story],
			OrderBy(Story.title, Ascending)
		)
/*
			OrderBy(Story.done, Ascending),
			OrderBy(Story.ready, Descending),
			OrderBy(Story.value, Descending),
			OrderBy(Story.complexity, Ascending),
*/

	private def title(story: Story, reDraw: () => JsCmd) =
		swappable(<span>{story.title}</span>,
			<span>{ajaxText(story.title, v => {story.title(v).save; reDraw()})}
			</span>)

	private def description(story: Story, reDraw: () => JsCmd) =
		swappable(<span>{story.description}</span>,
			<span>{ajaxText(story.description, v => {story.description(v).save; reDraw()})}
			</span>)

	private def delDependency(story: Story, dep: Story) = {
		story.dependsOn -= dep
		story.save
	}

	private def delTheme(story: Story, theme: Theme) = {
		story.themes -= theme
		story.save
	}

	private def themeList(story: Story, reDraw: () => JsCmd)(ns: NodeSeq): NodeSeq = {
		story.themes.flatMap({theme =>
			bind("theme", ns,
				"title" -> Text(theme.title.toString),
				"remove" -> ajaxButton("Remove", () => {delTheme(story, theme); reDraw()})
				)
		})
	}

	private def dependsOnList(story: Story, reDraw: () => JsCmd)(ns: NodeSeq): NodeSeq = {
		story.dependsOn.flatMap({dep => 
			bind("dep", ns,
				"title" -> Text(dep.title.toString),
				"remove" -> ajaxButton("Remove", () => {delDependency(story, dep); reDraw()})
				)
		})
	}

	private def addDep(story: Story, dep: Story) = {
		story.dependsOn += dep
		story.save
	}

	private def addTheme(story: Story, theme: Theme) = {
		story.themes += theme
		story.save
	}

	private def conditionalDeps(story: Story, reDraw: () => JsCmd)(html: NodeSeq): NodeSeq = {
		val visible = !story.dependsOn.isEmpty
		if (visible)
			bind("story", html,
				"dependsOn" -> dependsOnList(story, reDraw)_)
		else Nil
	}

	private def conditionalThemes(story: Story, reDraw: () => JsCmd)(html: NodeSeq): NodeSeq = {
		val visible = !story.themes.isEmpty
		if (visible)
			bind("story", html,
				"themes" -> themeList(story, reDraw)_)
		else Nil
	}

	private def doList(reDraw: () => JsCmd)(html: NodeSeq): NodeSeq =
		toShow.flatMap(story => bind("story", html,
			"done" -> ajaxCheckbox(story.done, v => {story.done(v).save; reDraw()}),
			"ready" -> ajaxCheckbox(story.ready, v => {story.ready(v).save; reDraw()}),
			"value" -> ajaxSelect(Story.valueList, Full(story.value.toString),
				v => {story.value(v.toInt).save; reDraw()}),
			"complexity" -> ajaxSelect(Story.complexityList, Full(story.complexity.toString),
				v => {story.complexity(v.toInt).save; reDraw()}),
			"title" -> title(story, reDraw),
			"link" -> link("/story", () => CurrentStoryVar(story), Text("View")),
			"nDeps" -> story.dependsOn.size,
			"nThemes" -> story.themes.size
		))

	private def storyList: List[(Story, String)] =
		(null, "(select one)") :: Story.storyList

	private def themesList: List[(Theme, String)] =
		(null, "(select one)") :: Theme.themeList

	def list (html: NodeSeq) = {
		val id = S.attr("stories_id").open_!

		def inner(): NodeSeq = {
			def reDraw() = SetHtml(id, inner())

			bind("story", html,
				"exclude" -> ajaxCheckbox(QueryNotDone, v => {QueryNotDone(v); reDraw}),
				"list" -> doList(reDraw) _)
		}

		inner()
	}
}

object QueryNotDone extends SessionVar(false)


// vim: set ts=4 sw=4 et:
