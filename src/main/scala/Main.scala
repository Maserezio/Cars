import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.liftweb.json._
import net.liftweb.json.JsonDSL._

import java.io.{File, PrintWriter}
import java.nio.file.Files
import scala.collection.mutable.ListBuffer

object Main {

  case class Car(name: String, spec: List[String], founded: String, founder: List[String], country: String, site: String)

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

    val listOfCars = getListOfCars(pages.head)
    for (i <- 1 until pages.size - 1)
      listOfCars ++= getListOfCars(pages(i))

    try {
      for(i<-listOfCars.indices)
      writeToFile(listOfCars(i).head.toString, parse(listOfCars(i)))
    } catch {
      case e: Exception => println("Error: %s".format(e.getMessage))
    } finally {
      println("Writing to JSON files finished!")
    }

  }


  private def getListOfCars(_page: String) = {
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
      val finalList = ListBuffer.empty ++= (page >> elementList(".brand-overview p").map(_ >> allText("p")))
        .filter(el => el.contains("Founded") || el.contains("Founder") || el.contains("Headquarters") || el.contains("Official Site"))

      val car = Car(
        preview.head.toString,
        preview(1).asInstanceOf[List[String]],
        if (finalList.find(_.contains("Founded:")) != None) getFounded(finalList.find(_.contains("Founded:")).get) else "",
        if (finalList.find(_.contains("Founder:")) != None) getFounder(finalList.find(_.contains("Founder: ")).get) else List.empty,
        if (finalList.find(_.contains("Headquarters:")) != None) getHeadquarters(finalList.find(_.contains("Headquarters:")).get) else "",
        if (finalList.find(_.contains("Official Site:")) != None) getSite(finalList.find(_.contains("Official Site:")).get) else ""
      )

      formJSON(car.name, car.spec, car.founded, car.founder, car.country, car.site)

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

      val car = Car(
        preview.head.toString,
        preview(1).asInstanceOf[List[String]],
        if (finalList.find(_.contains("Founded:")) != None) getFounded(finalList.find(_.contains("Founded:")).get) else "",
        if (finalList.find(_.contains("Founder:")) != None) getFounder(finalList.find(_.contains("Founder: ")).get) else List.empty,
        if (finalList.find(_.contains("Headquarters:")) != None) getHeadquarters(finalList.find(_.contains("Headquarters:")).get) else "",
        if (finalList.find(_.contains("Official Site:")) != None) getSite(finalList.find(_.contains("Official Site:")).get) else ""
      )

      formJSON(car.name, car.spec, car.founded, car.founder, car.country, car.site)

    }
  }

  private def formJSON(name: String, spec: List[String], founded: String, founder: List[String], country: String, site: String) = prettyRender(
    ("name" -> name) ~
      ("specialization" -> spec) ~
      ("founded" -> founded) ~
      ("founder" -> founder) ~
      ("country" -> country) ~
      ("site" -> site)
  )

  private def writeToFile(name: String, json: String): Unit = {
    val dir = new File("D:\\Uni\\NoSQL\\Cars\\JSON\\").mkdir()
    val pw = new PrintWriter(new File("D:\\Uni\\NoSQL\\Cars\\JSON\\" + s"${name}" + ".json"))
    pw.write(json)
    pw.close()
  }

  private def getFounded(str: String) = "\\d{4}".r.findFirstMatchIn(str).mkString("")

  private def getFounder(str: String) = str.substring(str.indexOf(':') + 2).split("(, )|( / )|( · )").toList

  private def getHeadquarters(str: String) = "([\\w\\s.]+)$".r.findAllMatchIn(str).mkString("").substring(1)

  private def getSite(str: String) = str.substring(str.indexOf(":") + 2)
}

