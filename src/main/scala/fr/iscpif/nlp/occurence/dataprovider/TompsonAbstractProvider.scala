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

  private def abstracts(lines: Stream[String]): Stream[() => String] =
    if(!lines.dropWhile(!_.startsWith("AB ")).head.startsWith("AB ")) Stream.empty
    else Stream.cons((mergeAbstract(lines) _), abstracts(lines.tail.dropWhile(_.startsWith("-- "))))
  
  def mergeAbstract(lines: Stream[String])() =
    Stream.cons(lines.head, lines.tail.takeWhile(_.startsWith("-- "))).map {
      l => l.slice(3, l.size)
    }.reduceLeft(_+_)
 
}
