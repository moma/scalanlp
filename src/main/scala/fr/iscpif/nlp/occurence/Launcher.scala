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
  val path = "/home/reuillon/Bureau/"
  val words = Source.fromFile(path + "FET-terms.txt").getLines.toList
  val text = "the patient record. Unfortunately, the methods by which these reports are generated are as diverse as the fiscal autonomy of academic clinical departments in a university-based health science center. In this paper, we reporton electronically capturing clinical reports, notes, and other text fragments from several hospital sources and many outpatient clinics autonomous robot. The purpose of microresonators the capture is to feed the ACIS (Advanced Clinical Information System) central patient data repository that soaring is in use at the University of Utah Health Sciences Center (UUHSC)." 
  val occurenceCounter = new OccurenceCounter(words)
  println(occurenceCounter(text).filter(_._2 > 0))
 }


