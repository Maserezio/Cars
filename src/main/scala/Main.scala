import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.liftweb.json._
import net.liftweb.json.JsonDSL._

import scala.collection.mutable.ListBuffer

object Main {

  val pages = List(
    "https://www.carlogos.org/car-brands/",
    "https://www.carlogos.org/car-brands/page-2.html",
    "https://www.carlogos.org/car-brands/page-3.html",
    "https://www.carlogos.org/car-brands/page-4.html",
    "https://www.carlogos.org/car-brands/page-5.html",
    "https://www.carlogos.org/car-brands/page-6.html",
    "https://www.carlogos.org/car-brands/page-7.html",
    "https://www.carlogos.org/car-brands/page-8.html",
  )

  def main(args: Array[String]) = {

    val listOfPages = getListOfCars(pages(0))
    listOfPages ++= getListOfCars(pages(1))
    listOfPages ++= getListOfCars(pages(2))
    listOfPages ++= getListOfCars(pages(3))
    listOfPages ++= getListOfCars(pages(4))
    listOfPages ++= getListOfCars(pages(5))
    listOfPages ++= getListOfCars(pages(6))
    listOfPages ++= getListOfCars(pages(7))

  val jsonList = ListBuffer[String]().empty
    for (i <- listOfPages.indices) {
      jsonList += parse(listOfPages(i))
    }
  }


  def getListOfCars(_page: String) = {
    val page = JsoupBrowser().get(_page)
    val names = ListBuffer.empty ++= (page >> elementList(".logo-list li a") >> attr("alt")("img"))
      .map(i => i.substring(0, i.length - 5))

    val links = ListBuffer.empty ++= (page >> elementList(".logo-list li") >> attr("href")("a"))

    val spans = ListBuffer.empty ++= (page >> elementList(".logo-list li a span").map(_ >> allText("span")))

    val (evenWithIndex, oddWithIndex) = spans.zipWithIndex.partition(_._2 % 2 == 0)
    val spec = evenWithIndex.map(_._1)
    val founded = oddWithIndex.map(_._1)

    val listOfPages = ListBuffer[List[Serializable]]().empty
    for (i <- names.indices) {
      if (founded(i).contains("Present") || founded(i).contains("present")) {
        listOfPages += Array(
          names(i),
          spec(i).split("(, )").toList,
          links(i).substring(11)
        ).toList
      }
    }
    listOfPages
  }

  private def parse(preview: List[Serializable]): String = {

    val page = JsoupBrowser().get("https://www.carlogos.org/car-brands" + s"${preview(2)}")

    if (page >> allText(".brand-overview p") != "") {
      val info = ListBuffer.empty ++= (page >> elementList(".brand-overview p").map(_ >> allText("p")))
        .filter(el => el.contains("Founded") || el.contains("Founder") || el.contains("Headquarters") || el.contains("Official Site"))

      val founded = if (info.find(_.contains("Founded:")) != None) getFounded(info.find(_.contains("Founded:")).get) else ""
      val founder = if (info.find(_.contains("Founder:")) != None) getFounder(info.find(_.contains("Founder: ")).get) else List.empty
      val head = if (info.find(_.contains("Headquarters:")) != None) getHeadquarters(info.find(_.contains("Headquarters:")).get) else ""
      val site = if (info.find(_.contains("Official Site:")) != None) getSite(info.find(_.contains("Official Site:")).get) else ""


      val json = ("name" -> preview.head.toString) ~
        ("specialization" -> preview(1).asInstanceOf[List[String]]) ~
        ("founded" -> founded) ~
        ("founder" -> founder) ~
        ("country" -> head) ~
        ("site" -> site)

      prettyRender(json)
    }
    else {
      val raw = (page >> elementList(".content table tbody tr").map(_ >> elementList("td"))).drop(1)
      val parsedRaw = ListBuffer[String]().empty
      for (i <- raw) {
        parsedRaw += i
          .mkString(": ")
          .replace("<br>", ", ")
          .replaceAll("(Production)|(MarfinalListts)", "Founded")
          .replaceAll("(Designer)|(Predecessor)", "Founder")
          .replaceAll("(Country)|(Assembly)", "Headquarters")
          .replace("Website", "Official Site")
          .replaceAll("(JsoupElement)|(<\\/a>)|(\\(<td>)|(\\(<td.+1\">)|(<\\/td>\\))|(<a.+\">)|(<\\/?p>)|(<\\/?strong>)|(&(nbsp|amp);)|",
            "")
      }
      val finalList = parsedRaw.filter(el => el.contains("Founded:") || el.contains("Founder:") || el.contains("Headquarters:") || el.contains("Official Site:"))

      val founded = if (finalList.find(_.contains("Founded:")) != None) getFounded(finalList.find(_.contains("Founded:")).get) else ""
      val founder = if (finalList.find(_.contains("Founder:")) != None) getFounder(finalList.find(_.contains("Founder:")).get) else List.empty
      val head = if (finalList.find(_.contains("Headquarters:")) != None) getHeadquarters(finalList.find(_.contains("Headquarters:")).get) else ""
      val site = if (finalList.find(_.contains("Official Site:")) != None) getSite(finalList.find(_.contains("Official Site:")).get) else ""

      val json = ("name" -> preview.head.toString) ~
        ("specialization" -> preview(1).asInstanceOf[List[String]]) ~
        ("founded" -> founded) ~
        ("founder" -> founder) ~
        ("country" -> head) ~
        ("site" -> site)

      prettyRender(json)
    }
  }

  private def getFounded(str: String) = "\\d{4}".r.findFirstMatchIn(str).mkString("")

  private def getFounder(str: String) = str.substring(str.indexOf(':') + 2).split("(, )|( / )|( · )").toList

  private def getHeadquarters(str: String) = "([\\w\\s.]+)$".r.findAllMatchIn(str).mkString("").substring(1)

  private def getSite(str: String) = str.substring(str.indexOf(":") + 2)
}

