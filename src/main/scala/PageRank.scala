//import scala.collection.parallel.CollectionConverters.*
import scala.annotation.tailrec
import scala.util.Random

object PageRank {
    /**
     * @param pages A map of page.id to page for some number of WebPage objects
     * @return      A map of page.id to a weight of 1.0 for those same WebPage objects
     */
    def equal(pages: Map[String, WebPage]): Map[String, Double] = {
        pages.keys.map(_ -> 1.0).toMap
    }

    /**
     * @param pages A map of page.id to page for some number of WebPage objects
     * @return A map of page.id to a weight that is a simple count of the number of pages linking to that page
     */
    def indegree(pages: Map[String, WebPage]): Map[String, Double] = {
        pages.map((key,value) => key -> (for page <- pages yield if page._2.links.contains(key) then 1 else 0).sum.toDouble)
    }


    def pagerank(pages: Map[String, WebPage]): Map[String, Double] = {
        val s = 10000
        val n = pages.size
        val numSteps = 100

        val pagesList = pages.values.toList

        val startPages = List.fill(s)(pagesList(Random.nextInt(n)))

        val endPages = startPages.map(runPick(_, pages, numSteps))


        Map()

    }

    @tailrec
    private def runPick(webPageId: String, pages: Map[String, WebPage], numSteps: Int): String = {
        val webPage = pages(webPageId)
        if numSteps <= 0 then webPageId else {
            val nextPageId: String = if webPage.links.nonEmpty && Random.nextDouble() < .85 then {
                webPage.links(Random().nextInt(webPage.links.size))
            } else {
                Random.shuffle(pages).head._1
            }
            runPick(nextPageId, pages, numSteps - 1)
        }
    }
}