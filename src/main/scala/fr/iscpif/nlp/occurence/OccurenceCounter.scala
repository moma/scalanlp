/*
 * Copyright (C) 2010 reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */


package fr.iscpif.nlp.occurence

import scala.actors.threadpool.AtomicInteger

class OccurenceCounter(dictionnary: Iterable[String], remove: Set[Char] = Set(',', '(', ')', ';', '.')) {
  
  def apply(text: String) = {
    val lowerText = text.toLowerCase.filterNot(remove)
    val index = Index(lowerText)

    val ret = Array.fill(dictionnary.size)(new AtomicInteger)
    
    val res = dictionnary.par.zipWithIndex.foreach {
      case(w, i) =>
        val indexes = index(w.split(' ').head)
        indexes.foreach {index => if(lowerText.slice(index, index + w.size) == w && endOfWord(lowerText, index + w.size)) ret(index).incrementAndGet}
    }
    ret.map{_.get}
  }
  
  private def endOfWord(text: String, pos: Int) = (pos == text.size) || (text(pos) == ' ' )
  
  private def splitExpressions(line: String) = line split("OR") map {_.trim.toLowerCase.filterNot(remove)}

}
