/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.nlp.occurence

import fr.iscpif.nlp.occurence.dataprovider.TompsonAbstractProvider
import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter
import scala.io.Source

object Compute {
  
  def apply(dico: File, articles: File, resFile: File) = {
    val words = Source.fromFile(dico)("UTF8")
    val occurenceCounter = 
      try new OccurenceCounter(words.getLines.map{_.trim}.toList)
      finally words.close

    val res = new BufferedWriter(new FileWriter (resFile))
    try {
      println("processing " + articles.getAbsolutePath)
      val base = Source.fromFile(articles)("UTF8")
      try {
        val tompson = new TompsonAbstractProvider(base.getLines.toStream)
        val result = tompson.apply.view.map {
          article => article.id -> occurenceCounter(article.content)
        }.foreach{v => 
          res.write(v._1 + " : " + 
                    v._2.toList.zipWithIndex.filterNot(_._1 == 0).map{case(a,b) => (b -> a).toString}.reduceLeftOption(_+", "+_).getOrElse("") +
                    '\n')
        }
      } finally base.close
    } finally res.close
  }
}
