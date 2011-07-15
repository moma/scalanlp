/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.iscpif.nlp.occurence

class OccurenceCounter(dictionnary: Iterable[String], remove: Set[Char] = Set(',', '(', ')', ';', '.')) {
    
  def apply(text: String) = {
    val lowerText = text.toLowerCase.filterNot(remove)
    val index = Index(lowerText)

    val occ = dictionnary.par.flatMap {
      case w =>
        index(w.split(' ').head).par.flatMap {
          index => if(lowerText.slice(index, index + w.size) == w && endOfWord(lowerText, index + w.size)) List(w) else List()
        }
    }.groupBy{w => w}.map{case(w, l) => w -> l.size}.seq
    
    dictionnary.zipWithIndex.par.map{
      case(l, i) => splitExpressions(l).map{w => occ.getOrElse(w, 0)}.reduce(_+_)
    }
  }
  
  private def endOfWord(text: String, pos: Int) = (pos == text.size) || (text(pos) == ' ' )
  
  private def splitExpressions(line: String) = line split("OR") map {_.trim.toLowerCase.filterNot(remove)}

}
