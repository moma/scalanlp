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

import scala.io.Source

object Launcher extends App {
  val remove = Set(',', '(', ')', ';', '.')

  val words = Source.fromFile("/home/reuillon/Bureau/FET-terms.txt").getLines.flatMap(splitExpressions).toList
  val text = "the patient record. Unfortunately, the methods by which these reports are generated are as diverse as the fiscal autonomy of academic clinical departments in a university-based health science center. In this paper, we reporton electronically capturing clinical reports, notes, and other text fragments from several hospital sources and many outpatient clinics autonomous robot. The purpose of microresonators the capture is to feed the ACIS (Advanced Clinical Information System) central patient data repository that soaring is in use at the University of Utah Health Sciences Center (UUHSC)." 
  
  val occ = occurences(text, words)
  
  val res = Source.fromFile("/home/reuillon/Bureau/FET-terms.txt").getLines.map{
    l => l -> splitExpressions(l).map{w => occ.getOrElse(w, 0)}.reduce(_+_)
  }
  
  println(res.filter(_._2 > 0).toList)
  
  def occurences(text: String, words: Iterable[String]) = {
    val lowerText = text.toLowerCase.filterNot(remove)
    val index = Index(lowerText)

    words.par.flatMap {
      w =>
      index(w.split(' ').head).par.flatMap {
        index => if(lowerText.slice(index, index + w.size) == w && endOfWord(lowerText, index + w.size)) List(w) else List()
      }
    }.groupBy(w => w).map{case(w, l) => w -> l.size}.seq
  }
  
  def endOfWord(text: String, pos: Int) = (pos == text.size - 1) || (text(pos) == ' ' )
  
  def splitExpressions(line: String) = line split("OR") map {_.trim.toLowerCase.filterNot(remove)}
}


