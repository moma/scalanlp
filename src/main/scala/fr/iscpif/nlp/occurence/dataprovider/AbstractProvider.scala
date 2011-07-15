/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.nlp.occurence.dataprovider

trait AbstractProvider {
  def apply: Iterable[String]
}
