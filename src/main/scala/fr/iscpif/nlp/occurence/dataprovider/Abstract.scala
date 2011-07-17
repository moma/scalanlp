/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.nlp.occurence.dataprovider

class Abstract(val content: String) {
  
  //lazy val content = _content()
  
  def id = {
    val md = java.security.MessageDigest.getInstance("SHA-1")
    new sun.misc.BASE64Encoder().encode(md.digest(content.getBytes))
  }

}
