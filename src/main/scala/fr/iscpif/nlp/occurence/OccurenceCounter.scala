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
import scalala.scalar._;
import scalala.tensor.::;
import scalala.tensor.mutable._;
import scalala.tensor.dense._;
import scalala.tensor.sparse._;
import scalala.library.Library._;
import scalala.library.LinearAlgebra._;
import scalala.library.Statistics._;
import scalala.library.Plotting._;
import scalala.operators.Implicits._;

class OccurenceCounter(dictionnary: Iterable[String], remove: Set[Char] = Set(',', '(', ')', ';', '.')) {
  
  def apply(text: String) = {
    val lowerText = text.toLowerCase.filterNot(remove)
    val index = Index(lowerText)

    val vect = SparseVector.zeros[Int](dictionnary.size) 
    dictionnary.zipWithIndex.flatMap {
      case(w, i) =>
        val splited = w.split(' ')
        val indexes = index(splited.slice(0, 2).reduceLeft(_ + " " + _))
        indexes.flatMap {
          index => 
            if(splited.size <= 2 || lowerText.slice(index, index + w.size) == w && endOfWord(lowerText, index + w.size)) List(i)
            else List.empty
          }
    }.seq.foreach{v => vect(v) += 1}
    vect
  }
  
  private def endOfWord(text: String, pos: Int) = (pos == text.size) || (text(pos) == ' ' )
   
  private def splitExpressions(line: String) = line split("OR") map {_.trim.toLowerCase.filterNot(remove)}

}
