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

import com.csvreader.CsvWriter
import java.io.StringWriter

class CSVOutput(themes: List[Theme], stories: List[Story]) {
	private def commaSeparate(l: Seq[String]) = {
		if (l.isEmpty) ""
		else if (l.size == 1) l(0)
		else l.reduceLeft((a, b) => a + ", " + b)
	}

	private def indexList(l: List[Story]) =
		l.map(dep => (stories.indexOf(dep) + 1).toString)

	private def dependsOn(story: Story): String = {
		commaSeparate(indexList(story.dependsOn.toList))
	}

	private def included(theme: Theme): String = {
		commaSeparate(indexList(removeDone(theme.stories.toList)))
	}

	private def allIncluded(l0: List[Story]): Boolean = l0 match {
		case s :: l1 =>
			if (stories.contains(s)) allIncluded(l1)
			else false
		case List() =>
			true
	}

	private def removeDone(l0: List[Story]): List[Story] = {
		l0.filter(s => !s.done)
	}

	private def allowable(theme: Theme): Boolean = {
		if (removeDone(theme.stories.toList).isEmpty) false
		else allIncluded(removeDone(theme.stories.toList))
	}

	private def filterThemes: List[Theme] = {
		for {
			theme <- themes
			if (allowable(theme))
		} yield theme
	}

	def themeCSV: String = {
		val writer = new StringWriter()
		val csv = new CsvWriter(writer, ',')
		csv.write("Theme")
		csv.write("Value")
		csv.write("Included stories")
		csv.endRecord()
		for (theme <- filterThemes) {
			csv.write(theme.title.toString)
			csv.write(theme.value.toString)
			csv.write(included(theme))
			csv.endRecord()
		}
		csv.close()
		writer.toString()
	}

	def storyCSV: String = {
		val writer = new StringWriter()
		val csv = new CsvWriter(writer, ',')
		csv.write("Story")
		csv.write("Value")
		csv.write("Effort")
		csv.write("Depends on")
		csv.endRecord()
		for (story <- stories) {
			csv.write(story.title.toString)
			csv.write(story.value.toString)
			csv.write(story.complexity.toString)
			csv.write(dependsOn(story))
			csv.endRecord()
		}
		csv.close()
		writer.toString()
	}
}

class OutputSnip {
	private def getThemes: List[Theme] = {
		Theme.findAll()
	}

	private def getStories: List[Story] = {
		Story.findAll(By(Story.ready, true), By(Story.done, false))
	}

	def view(html: NodeSeq) = {
		val CSV = new CSVOutput(getThemes, getStories)
		bind("output", html,
			"stories" -> textarea(CSV.storyCSV, v=>{}),
			"themes" -> textarea(CSV.themeCSV, v=>{})
		)
	}
}

// vim: set ts=4 sw=4 et:
