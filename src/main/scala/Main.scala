import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._

import net.liftweb.json._
import net.liftweb.json.JsonDSL._

import scala.collection.mutable.ListBuffer

object Main {

  def main(args: Array[String]) = {
    val browser = JsoupBrowser()
    val page = browser.get("https://www.carlogos.org/car-brands/bmw-logo.html")

    val info = ListBuffer.empty ++= (page >> elementList(".brand-overview p").map(_ >> allText("p")))
      .filter(el => el.contains("Founded") || el.contains("Founder") || el.contains("Headquarters") || el.contains("Official Site"))

    val founded = getFounded(info(0))
    val founder = getFounder(info(1))
    val head = getHeadquarters(info(2))
    val site = getSite(info(3))


    val id = 1
    val name = "Honda"
    val specialization = "Mass-Market Cars"

    val json = ("id" -> id)~
      ("name" ->name)~
      ("specialization" -> specialization)~
      ("founded" -> founded)~
      ("founder" -> founder)~
      ("country" -> head)~
      ("site" -> site)
    println(prettyRender(json))
    
  }

  private def getFounded(str: String) = str.substring(str.indexOf(":")+2)
  private def getFounder(str: String) = str.substring(str.indexOf(':')+2).split("(, )|( / )").toList
  private def getHeadquarters(str: String) = "([\\w\\s.]+)$".r.findAllMatchIn(str).mkString("").substring(1)
  private def getSite(str: String) = str.substring(str.indexOf(":")+2)
}

