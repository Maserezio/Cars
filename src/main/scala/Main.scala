import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import scala.collection.mutable.ListBuffer

object Main {

  def main(args: Array[String]) = {
    val browser = JsoupBrowser()
    val page = browser.get("https://www.carlogos.org/car-brands/")

    val names = ListBuffer.empty ++= (page >> elementList(".logo-list li a") >> attr("alt")("img"))
      .map(i => i.substring(0, i.length - 5))

    val links = ListBuffer.empty ++= (page >> elementList(".logo-list li") >> attr("href")("a"))

    val spans = ListBuffer.empty ++= (page >> elementList(".logo-list li a span").map(_ >> allText("span")))

    val (evenWithIndex, oddWithIndex) = spans.zipWithIndex.partition(_._2 % 2 == 0)
    val spec = evenWithIndex.map(_._1)
    val founded = oddWithIndex.map(_._1)

    val kek = ListBuffer[List[Serializable]]().empty

    for (i <- names.indices) {
      if (founded(i).contains("Present") || founded(i).contains("present")) {
        kek += Array(
          names(i),
          spec(i).split("(, )").toList,
          links(i).substring(11)
        ).toList
      }
    }

    kek.toList

    parse(kek(0))
  }

  private def parse(preview: List[Serializable]): Unit = {
    val browser = JsoupBrowser()
    val page = browser.get("https://www.carlogos.org/car-brands" + s"${preview(2)}")

    val info = ListBuffer.empty ++= (page >> elementList(".brand-overview p").map(_ >> allText("p")))
      .filter(el => el.contains("Founded") || el.contains("Founder") || el.contains("Headquarters") || el.contains("Official Site"))

    val founded = getFounded(info.head)
    val founder = getFounder(info(1))
    val head = getHeadquarters(info(2))
    val site = getSite(info(3))


    val json = ("name" -> preview.head.toString) ~
      ("specialization" -> preview(1).asInstanceOf[List[String]]) ~
      ("founded" -> founded) ~
      ("founder" -> founder) ~
      ("country" -> head) ~
      ("site" -> site)
    println(prettyRender(json))
  }

  private def getFounded(str: String) = str.substring(str.indexOf(":") + 2)

  private def getFounder(str: String) = str.substring(str.indexOf(':') + 2).split("(, )|( / )").toList

  private def getHeadquarters(str: String) = "([\\w\\s.]+)$".r.findAllMatchIn(str).mkString("").substring(1)

  private def getSite(str: String) = str.substring(str.indexOf(":") + 2)
}

