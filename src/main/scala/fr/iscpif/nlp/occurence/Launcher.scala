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

import fr.iscpif.nlp.occurence.dataprovider.TompsonAbstractProvider
import scala.io.Source
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

object Launcher extends App {
 // val path = "/data/tina/Thomson/raw/IF3N101426on/Bureau/"
  val words = Source.fromFile("/data/tina/Thomson_analysis/FET-terms.txt")("UTF8").getLines.map{_.trim}.toList
  val text = "the patient record. Unfortunately, the methods by which these reports are generated are as diverse as the fiscal autonomy of academic clinical departments in a university-based health science center. In this paper, we reporton electronically capturing clinical reports, notes, and other text fragments from several hospital sources and many outpatient clinics autonomous robot. The purpose of microresonators the capture is to feed the ACIS (Advanced Clinical Information System) central patient data repository that soaring is in use at the University of Utah Health Sciences Center (UUHSC)." 
  //val indexBuilder = Index(_: String, (s: String) => Math.abs(s.hashCode % 5000))
  val occurenceCounter = new OccurenceCounter(words)
  
  val tompson = new TompsonAbstractProvider(Source.fromFile("/data/tina/Thomson/raw/IF3N101426")("UTF8").getLines.toStream)
  
  val begin = System.currentTimeMillis
  
  val result = tompson.apply.view.map {
    article => occurenceCounter(article())
  }.reduce(_+_)
  
  println("time: " + (System.currentTimeMillis - begin))
  println(result)
  
 }


