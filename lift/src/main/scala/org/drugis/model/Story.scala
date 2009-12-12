package org.drugis.model

import net.liftweb._
import common._
import mapper._
import http._
import SHtml._
import util._
import sitemap._
import sitemap.Loc._

class Story extends LongKeyedMapper[Story]
with IdPK
with ManyToMany {
	def getSingleton = Story

	object title extends MappedString(this, 255)
	object description extends MappedText(this)
	object createdBy extends MappedLongForeignKey(this, User)
	object value extends MappedInt(this) {
		override def defaultValue = 0
		override def validations = validValue _ :: super.validations

		def validValue(in: Int): List[FieldError] =
		if (in >= 0 && in <= 5) Nil
		else List(FieldError(this, <b>Value must be in {Story.valueList.toString}</b>))

		override def _toForm = Full(select(Story.valueList, Full(is.toString),
			f => set(f.toInt)))
	}
	object complexity extends MappedInt(this) {
		override def defaultValue = 0
		override def validations = validComplexity _ :: super.validations

		def validComplexity(in: Int): List[FieldError] =
		if (in == 0 || Story.complexityList.contains((in.toString, in.toString))) Nil
		else List(FieldError(this, <b>Complexity must be in {Story.complexityList.toString}</b>))

		override def _toForm = Full(select(Story.complexityList,
			Full(is.toString),
			f => set(f.toInt)))
	}
	object ready extends MappedBoolean(this)
	object done extends MappedBoolean(this)
	object themes extends MappedLongForeignKey(this, ThemeStory)
	object dependsOn extends MappedManyToMany(StoryPrecedence, StoryPrecedence.antecedent, StoryPrecedence.precedent, Story)
}

object Story extends Story with LongKeyedMetaMapper[Story] {
	lazy val valueList = ("0", "NA") :: ((1 to 5).map(v => (v.toString, v.toString))).toList
	lazy val complexityList = ("0", "NA") :: List(1, 2, 3, 5, 8).map(v => (v.toString, v.toString)).toList
	lazy val sitemap : List[Menu] = List(Menu(Loc("Stories", List("stories"), "Stories")))
	def storyList = findAll().map({story:Story => (story, story.title.toString)})
}

class StoryPrecedence extends LongKeyedMapper[StoryPrecedence]
with IdPK {
	def getSingleton = StoryPrecedence

	object precedent extends MappedLongForeignKey(this, Story)
	object antecedent extends MappedLongForeignKey(this, Story)
}

object StoryPrecedence extends StoryPrecedence
with LongKeyedMetaMapper[StoryPrecedence]

// vim: set ts=4 sw=4 et:
