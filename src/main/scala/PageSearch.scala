import scala.collection.parallel.CollectionConverters.*
import scala.math.log

object PageSearch {
    /**
     * @param pages  a list of RankedWebPage objects to be searched
     * @param query  a list of search terms to be counted in those pages
     * @return       a list of the number of times any of the terms appeared in each page in the same order as given
     */
    def count(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        for page <- pages yield {
            (for term <- query yield page.text.toLowerCase.sliding(term.length).count(_ == term.toLowerCase)).sum.toDouble
        }
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the term-frequency of the occurrences of those terms in each page in the same order given
     */
    def tf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        val countVals: List[Double] = count(pages, query)
        for (page, countVal) <- pages zip countVals yield countVal / page.text.length
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the TF-IDF score for each page in the same order given
     */
    def tfidf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        for page <- pages yield {
            query.par.map(term => {
                val numPagesContains = pages.count(_.text.toLowerCase.contains(term))
                val tfVal = tf(List(page), List(term)).head
                val idfVal = log(pages.length / (1.0 + numPagesContains))
                tfVal * idfVal
            }).sum
        }
    }
}