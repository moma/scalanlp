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

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.MapBuilder

object Index {
  def apply(text: String) = {
    
    def rewriteWord(word: List[Char]) = (new StringBuilder ++= word.reverse).toString
    
    @tailrec def words(letters: IndexedSeq[(Char, Int)], acc: List[(String, Int)] = List.empty, word: List[Char] = List.empty): List[(String, Int)] = 
      letters head match {
        case (c, pos) =>
          if(letters.tail.isEmpty) rewriteWord(word) -> (pos - word.size) :: acc
          else c match {
            case ' ' => words(letters.tail, rewriteWord(word) -> (pos - word.size) :: acc)
            case c => words(letters.tail, acc,c :: word)
          }
      }
    
    val wordsPositions = words(text zipWithIndex /*.toLowerCase.zipWithIndex.filterNot(c => remove.contains(c._1))*/) 
    //println(wordsPositions)
    val by2 = wordsPositions.grouped(2).filter(_.size == 2).map{e => val f = e(0); val l = e(1); (f._1 + " " + l._1) -> f._2}
    val by2i = wordsPositions.tail.grouped(2).filter(_.size == 2).map{e => val f = e(0); val l = e(1); (f._1 + " " + l._1) -> f._2}
    val map = (wordsPositions ++ by2 ++ by2i).foldLeft(TreeMap.empty[String, ListBuffer[Int]]) {
      case(m, (word, pos)) => 
        m.get(word) match {
          case None => m + (word -> ListBuffer(pos))
          case Some(l) => l += pos; m
        }
    }

    
    new Index(map)
  }
  
}

class Index(val content: Map[String, Iterable[Int]]) {
  def apply(word: String) = content.getOrElse(word, Iterable.empty)
  override def toString = content.toString
}
