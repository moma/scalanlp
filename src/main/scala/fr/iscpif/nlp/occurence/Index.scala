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

import scala.collection.immutable.TreeMap
import scala.collection.mutable.ListBuffer

object Index {
  def apply(text: String) = {
    
    def words(letters: IndexedSeq[(Char, Int)], word: List[Char] = List.empty): List[(String, Int)] = 
      letters headOption match {
        case Some((' ', pos)) => (new StringBuilder() ++= word.reverse).toString -> (pos - word.size) :: words(letters.tail)
        case Some((c, pos)) => words(letters.tail, c :: word)
        case None => List.empty
      }
    
    val wordsPositions = words(text zipWithIndex /*.toLowerCase.zipWithIndex.filterNot(c => remove.contains(c._1))*/) 
      
    val map: TreeMap[String, Iterable[Int]] = wordsPositions.foldLeft(new TreeMap[String, ListBuffer[Int]]) {
      case(map, (word, pos)) => map.get(word) match {
          case None => map + (word -> ListBuffer(pos))
          case Some(e) => e += pos; map
        } 
    }
    
    new Index(map)
  }
  
}

class Index(val content: TreeMap[String, Iterable[Int]]) {
  def apply(word: String) = content.getOrElse(word, Iterable.empty)
  override def toString = content.toString
}
