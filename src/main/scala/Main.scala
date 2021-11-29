import ch.qos.logback.classic.{Level, LoggerContext}
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import Helpers._

import java.io.{File, PrintWriter}
import scala.collection.mutable.ListBuffer
import org.mongodb.scala._
import org.mongodb.scala.bson.BsonDocument
import org.mongodb.scala.model.Filters.{and, equal, exists, lt, lte, or, size}
import org.mongodb.scala.model.Indexes.descending
import org.mongodb.scala.model.Projections.{exclude, excludeId, fields}
import org.slf4j.LoggerFactory

import scala.io.Source


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

//    try {
//      for (i <- listOfCars.indices)
//        writeToFile(listOfCars(i).head.toString, parse(listOfCars(i)))
//    } catch {
//      case e: Exception => println("Error: %s".format(e.getMessage))
//    } finally {
//      println("Writing to JSON files finished!")
//    }

    writeToDB()
    queries()

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
        if (finalList.exists(_.contains("Founded:"))) getFounded(finalList.find(_.contains("Founded:")).get) else "",
        if (finalList.exists(_.contains("Founder:"))) getFounder(finalList.find(_.contains("Founder: ")).get) else List.empty,
        if (finalList.exists(_.contains("Headquarters:"))) getHeadquarters(finalList.find(_.contains("Headquarters:")).get) else "",
        if (finalList.exists(_.contains("Official Site:"))) getSite(finalList.find(_.contains("Official Site:")).get) else ""
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
        if (finalList.exists(_.contains("Founded:"))) getFounded(finalList.find(_.contains("Founded:")).get) else "",
        if (finalList.exists(_.contains("Founder:"))) getFounder(finalList.find(_.contains("Founder: ")).get) else List.empty,
        if (finalList.exists(_.contains("Headquarters:"))) getHeadquarters(finalList.find(_.contains("Headquarters:")).get) else "",
        if (finalList.exists(_.contains("Official Site:"))) getSite(finalList.find(_.contains("Official Site:")).get) else ""
      )

      formJSON(car.name, car.spec, car.founded, car.founder, car.country, car.site)

    }
  }

  private def formJSON(name: String, spec: List[String], founded: String, founder: List[String], country: String, site: String) = compactRender(
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

  private def writeToDB(): Unit = {
    val loggerContext = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
    val rootLogger = loggerContext.getLogger("org.mongodb.driver")
    val rootLogger2 = loggerContext.getLogger("reactor.util.Loggers")
    rootLogger.setLevel(Level.OFF)
    rootLogger2.setLevel(Level.OFF)

    val uri = "mongodb://localhost:27017"
    val client: MongoClient = MongoClient(uri)
    val db: MongoDatabase = client.getDatabase("cars")
    val collection: MongoCollection[Document] = db.getCollection("brands")

    val files = getListOfFiles("D:\\Uni\\NoSQL\\Cars\\JSON")
    for (i <- files.indices) {
      val tmp = Source.fromFile(files(i))
      collection.insertOne(BsonDocument(tmp.mkString)).results()
      tmp.close()
    }
    client.close()
    println("Written to MongoDB!")
  }

  private def c() = {
    val uri = "mongodb://localhost:27017"
    val client: MongoClient = MongoClient(uri)
    val db: MongoDatabase = client.getDatabase("cars")
    val collection: MongoCollection[Document] = db.getCollection("brands")

    println("\nЗапросы\n")

    println("Вывести немецкие бренды производителей")
    collection
      .find(equal("country", "Germany"))
      .limit(5)
      .projection(fields(exclude("country"), excludeId()))
      .printResults()
    println("")

    println("Вывести бренды, у которых больше одного создателя")
    collection
      .find(equal("founded", "1941"))
      .projection(excludeId())
      .printResults()
    println("")

    println("Вывести все бренды, созданные в 1941 год")
    collection
      .find(exists("founder.1", exists = true))
      .projection(excludeId())
      .sort(descending("name"))
      .limit(4)
      .printResults()
    println("")

    println("Вывести американские бренды, созданные до 1950")
    collection.find(
      or(
        and(equal("country", "U.S."), lt("founded", "1950")),
        and(equal("country", "United States"), lt("founded", "1950"))
      ))
      .projection(excludeId())
      .limit(5)
      .printResults()
    println("")

    println("Вывести все итальянские автомобили со специализацией «Роскошные автомобили», основанные до 1920 одним человеком")
    collection.find(
      or(
        and(equal("specialization", "Luxury Vehicles"), equal("country", "Italy"), size("founder", 1), lte("founded", "1920")),
        and(equal("specialization", "Luxury Cars"), equal("country", "Italy"), size("founder", 1), lte("founded", "1920"))
      ))
      .projection(excludeId())
      .printResults()
    println("")
  }

  private def getListOfFiles(dir: String): List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }

}

