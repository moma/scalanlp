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

import scala.annotation.tailrec

class TompsonAbstractProvider(file: Iterable[String]) extends AbstractProvider {
  override def apply = abstracts(file)

  @tailrec final private def abstracts(lines: Iterable[String], acc: List[String] = List.empty): List[String]  = 
    lines.headOption match {
      case Some(l) => 
        if(l.startsWith("AB ")) {
          val abs = mergeAbstract(lines)
          abstracts(lines.drop(abs._2), abs._1 :: acc)
        } else abstracts(lines.tail, acc)
      case None => acc
    }
  
  def mergeAbstract(lines: Iterable[String], curAbstract: List[String] = List.empty) = {
    val abstractLines = (List(lines.head) ++  lines.tail.takeWhile(_.startsWith("-- "))).map {
      l => l.slice(3, l.size)
    }
    abstractLines.reduceLeft(_+_) -> abstractLines.size
  }
 
}
