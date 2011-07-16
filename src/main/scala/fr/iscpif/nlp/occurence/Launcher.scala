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
import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter
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
  val words = Source.fromFile("/data/tina/Thomson_analysis/FET-terms.txt")("UTF8")
  val occurenceCounter = 
    try new OccurenceCounter(words.getLines.map{_.trim}.toList)
  finally words.close
 
  val tompson = new TompsonAbstractProvider(Source.fromFile("/data/tina/Thomson/raw/IF3N101426")("UTF8").getLines.toStream)
  val dir = new File("/data/tina/Thomson/raw/")
  val begin = System.currentTimeMillis

  val res = new BufferedWriter(new FileWriter ("/iscpif/users/reuillon/Desktop/thomson-occ.txt"))
  try {
    dir.listFiles.foreach {
      file => 
      println("processing " + file.getAbsolutePath)
      val base = Source.fromFile(file)("UTF8")
      try {
        val tompson = new TompsonAbstractProvider(base.getLines.toStream)
        val result = tompson.apply.view.map {
          article => article.ut -> occurenceCounter(article.content())
        }.foreach{v => 
          res.write(v._1 + " : " + 
                    v._2.toList.zipWithIndex.filterNot(_._1 == 0).map{case(a,b) => (b -> a).toString}.reduceLeftOption(_+", "+_).getOrElse("") +
                    '\n')
        }
      } finally base.close
    }
  }finally res.close
  println("time: " + (System.currentTimeMillis - begin))

}


