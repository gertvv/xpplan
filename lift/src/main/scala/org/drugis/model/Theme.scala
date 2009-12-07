package org.drugis.model

import net.liftweb._
import mapper._
import http._
import SHtml._
import util._
import sitemap._
import sitemap.Loc._

class Theme extends LongKeyedMapper[Theme] with IdPK {
	def getSingleton = Theme

	object title extends MappedText(this)
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
}

object Theme extends Theme with LongKeyedMetaMapper[Theme] {
	lazy val valueList = (1 to 50).map(v => (v.toString, v.toString))
	lazy val sitemap : List[Menu] = List(Menu(Loc("Themes", List("themes"), "Themes")))
}

// vim: set ts=4 sw=4 et:
