package org.drugis.model

import net.liftweb._
import mapper._
import http._
import SHtml._
import util._
import sitemap._
import sitemap.Loc._

class Story extends LongKeyedMapper[Story] with IdPK {
	def getSingleton = Story

	object title extends MappedText(this)
	object description extends MappedText(this)
	object createdBy extends MappedLongForeignKey(this, User)
	object value extends MappedInt(this) {
		override def defaultValue = 4
		override def validations = validValue _ :: super.validations

		def validValue(in: Int): List[FieldError] =
		if (in > 0 && in <= 5) Nil
		else List(FieldError(this, <b>Value must be in {Story.valueList.toString}</b>))

		override def _toForm = Full(select(Story.valueList, Full(is.toString),
			f => set(f.toInt)))
	}
	object complexity extends MappedInt(this) {
		override def defaultValue = 2
		override def validations = validComplexity _ :: super.validations

		def validComplexity(in: Int): List[FieldError] =
		if (Story.complexityList.contains((in.toString, in.toString))) Nil
		else List(FieldError(this, <b>Complexity must be in {Story.complexityList.toString}</b>))

		override def _toForm = Full(select(Story.complexityList,
			Full(is.toString),
			f => set(f.toInt)))
	}
	object done extends MappedBoolean(this)
}

object Story extends Story with LongKeyedMetaMapper[Story] {
	lazy val valueList = (1 to 5).map(v => (v.toString, v.toString))
	lazy val complexityList = List(1, 2, 3, 5, 8).map(v => (v.toString, v.toString))
	lazy val sitemap : List[Menu] = List(Menu(Loc("Stories", List("stories"), "Stories")))
}

// vim: set ts=4 sw=4 et:
