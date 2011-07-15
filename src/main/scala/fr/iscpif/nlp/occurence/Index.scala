/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.nlp.occurence

import scala.collection.immutable.TreeMap
import scala.collection.mutable.ListBuffer

object Index {
  def apply(text: String) = {
    
    def words(letters: IndexedSeq[(Char, Int)], word: List[Char] = List.empty): List[(String, Int)] = 
      letters headOption match {
        case None => List.empty
        case Some((' ', pos)) => (new StringBuilder() ++= word.reverse).toString -> (pos - word.size) :: words(letters.tail)
        case Some((c, pos)) => words(letters.tail, c :: word)
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
