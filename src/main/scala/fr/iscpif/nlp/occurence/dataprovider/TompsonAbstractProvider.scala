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


package fr.iscpif.nlp.occurence.dataprovider


class TompsonAbstractProvider(file: Stream[String]) extends AbstractProvider {
  override def apply = abstracts(file)

  private def abstracts(lines: Stream[String]): Stream[Abstract] = {
    val newAB = lines.dropWhile(!_.startsWith("AB "))
    if(!newAB.head.startsWith("AB ")) Stream.empty
    else {
      //val ut = newAB.head.slice(3, newUT.head.size)
      //val newArticle = lines.dropWhile(!_.startsWith("AB "))
      val abs = merge(newAB) _
      Stream.cons(new Abstract(abs), abstracts(newAB.tail.dropWhile(_.startsWith("-- "))))
    }
  }
  
  def merge(lines: Stream[String])() =
    Stream.cons(lines.head, lines.tail.takeWhile(_.startsWith("-- "))).map {
      l => l.slice(3, l.size)
    }.reduceLeft(_+_)
 
  
}
