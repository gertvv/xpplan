package org.drugis.model

import net.liftweb._
import common._
import mapper._
import http._
import SHtml._
import util._
import sitemap._
import sitemap.Loc._

class Theme extends LongKeyedMapper[Theme]
with IdPK
with ManyToMany {
	def getSingleton = Theme

	object title extends MappedString(this, 255)
	object description extends MappedText(this)
	object createdBy extends MappedLongForeignKey(this, User)
	object value extends MappedInt(this) {
		override def defaultValue = 10
		override def validations = validValue _ :: super.validations

		def validValue(in: Int): List[FieldError] =
		if (in > 0 && in <= 50) Nil
		else List(FieldError(this, <b>Value must be in {Theme.valueList.toString}</b>))

		override def _toForm = Full(select(Theme.valueList, Full(is.toString),
			f => set(f.toInt)))
	}
	object stories extends MappedManyToMany(ThemeStory, ThemeStory.theme, ThemeStory.story, Story)

	def totalStoryValue = {
		val values = for {
			story <- stories
		} yield story.value.toInt

		if (values.size == 0) 0
		else values.reduceLeft[Int](_ + _)
	}
}

object Theme extends Theme with LongKeyedMetaMapper[Theme] {
	lazy val valueList = (1 to 50).map(v => (v.toString, v.toString))
	lazy val sitemap : List[Menu] = List(Menu(Loc("Themes", List("themes"), "Themes")), Menu(Loc("Theme", List("theme"), "View Theme", Hidden)))
	def themeList = findAll(OrderBy(Theme.title, Ascending)).map({theme:Theme => (theme, theme.title.toString)})
}

class ThemeStory extends LongKeyedMapper[ThemeStory]
with IdPK {
	def getSingleton = ThemeStory

	object theme extends MappedLongForeignKey(this, Theme)
	object story extends MappedLongForeignKey(this, Story)
}

object ThemeStory extends ThemeStory with LongKeyedMetaMapper[ThemeStory]

// vim: set ts=4 sw=4 et:
