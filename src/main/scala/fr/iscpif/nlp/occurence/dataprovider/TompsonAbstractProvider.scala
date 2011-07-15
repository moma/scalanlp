/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
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
