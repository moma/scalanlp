/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.nlp.occurence

import scala.collection.BitSet

object BloomFilter {
 
  def apply(words: Iterable[String], hash: String => Int) = 
    new BloomFilter(BitSet(words.map{hash}.toSeq: _*), hash)
}

class BloomFilter(val footPrint: BitSet, val hash: String => Int) {
  def apply(word: String) = footPrint.contains(hash(word))
}

