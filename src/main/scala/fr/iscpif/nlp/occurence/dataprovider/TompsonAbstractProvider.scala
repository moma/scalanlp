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


class TompsonAbstractProvider extends AbstractProvider {
  override def apply(file: Iterator[String]) = abstracts(file)

  private def abstracts(lines: Iterator[String]): Iterator[Abstract] = {
   val abIt = lines.dropWhile(!_.startsWith("AB "))
    if(!abIt.hasNext) Iterator.empty
    else {
      val tailAB = abIt.takeWhile(_.startsWith("-- "))
      val abs = 
        (Iterator.single(abIt.next) ++ tailAB).map {
          l => l.slice(3, l.size)
        }.reduceLeft(_+_)
      //println(abs)
      Iterator.single(new Abstract(abs)) ++ abstracts(abIt.dropWhile(_.startsWith("-- ")))
    }
  }
  

 
  
}
