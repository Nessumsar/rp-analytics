package controllers

import java.nio.charset.StandardCharsets

import javax.inject._
import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.i18n.Lang.logger
import play.api.i18n._

import scala.util.matching.Regex

@Singleton
class HomeController @Inject()(val controllerComponents: ControllerComponents, override val messagesApi: MessagesApi) extends BaseController with I18nSupport {

  case class TextData(rawText: String)

  val textForm: Form[TextData] = Form(
    mapping(
      "rawText" -> nonEmptyText
    )(TextData.apply)(TextData.unapply)
  )

  def index() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index(null, null))
  }

  def analyzeText() = Action { implicit request: Request[AnyContent] =>
    textForm.bindFromRequest.fold(
      formWithErrors => {
        logger.error("Form binding failed")
        BadRequest("Form binding failed")
      },
      formData => {
        val rawText = new String(formData.rawText.getBytes(StandardCharsets.ISO_8859_1), StandardCharsets.UTF_8)
        val returningUnits = analyzeRawText(rawText)
        println(s"Size: ${returningUnits.size}")
        println(s"Units: ${returningUnits.last.units}")
        println(s"Bounty: ${returningUnits.last.bounty}")
        println(s"Arrival Time: ${returningUnits.last.arrivalTime}")
        Ok(views.html.index(returningUnits, summarizeReturningTroops(returningUnits)))
      }
    )
  }

  def analyzeRawText(rawText: String): List[ReturningUnit] = {
    val cleanedText = rawText
      .replaceAll("[^\\x00-\\x7F]+", "")   // Remove non-ASCII characters
      .replaceAll("\\s+", " ")              // Replace multiple spaces with a single space
      .replaceAll("\\?", "")                // Remove question marks from getting rawText as UTF-8

    // Define regex patterns for the relevant parts
    val segmentPattern: Regex = """\(\d+\)[^\n]*?Troops\s*(.*?)\s*Bounty\s*(.*?)\s*/\s*\d+""".r
    val troopsPattern: Regex = """Troops\s+((\d+\s+){11})Bounty""".r
    val arrivalTimePattern: Regex = """Arrival in [0-9:]+ hrs\.at ([0-9:]+)""".r

    // Extract all relevant segments for troops, bounty, and arrival time
    val segmentMatches = segmentPattern.findAllMatchIn(cleanedText).toList
    // Extract troops, bounty, and arrival time from each segment

    // Split the text into sections based on the "Incoming troops" delimiter
    val sections = removeFirstAndConditionalLast(cleanedText.split("Return from").filter(_.nonEmpty).toList, _.contains("Outgoing troops"))

    // Process each section
    sections.flatMap { section =>
      // Extract all numbers between "Troops" and "Bounty"
      val troopsMatch = troopsPattern.findFirstMatchIn(section)

      val units = troopsMatch.map { m =>
        m.group(1).trim.split("\\s+").map(_.toInt).toList
      }.getOrElse(List.empty)
      // Extract bounty numbers (all numbers before the slash)
      val bountyPattern: Regex = """Bounty\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s*/\s*\d+""".r
      val bountyMatch = bountyPattern.findFirstMatchIn(section)


      val bounty = bountyMatch.map { m =>
        Bounty(
          wood = m.group(1).toInt,
          clay = m.group(2).toInt,
          iron = m.group(3).toInt,
          wheat = m.group(4).toInt,
          total = m.group(5).toInt
        )
      }.getOrElse(Bounty(0, 0, 0, 0, 0)) // Handle case where no match is found
      logger.info(s"${section}\n")


      // Extract arrival time
      val arrivalTimeMatch = arrivalTimePattern.findFirstMatchIn(section)
      val arrivalTime = arrivalTimeMatch.map(_.group(1)).getOrElse("N/A")

      // Create the ReturningUnits object
      Some(ReturningUnit(
        units = units.sum,
        bounty = bounty,
        arrivalTime = arrivalTime
      ))
    }

  }


  def removeFirstAndConditionalLast(list: List[String], condition: String => Boolean): List[String] = {
    list match {
      case Nil => Nil
      case _ if list.tail.isEmpty => list.tail // Only one element in the list, remove it
      case _ =>
        val withoutFirst = list.tail
        if (condition(withoutFirst.last)) withoutFirst.init else withoutFirst
    }
  }

  def summarizeReturningTroops(list: List[ReturningUnit]) : ReturningUnit = {
    var units = 0
    var wood = 0
    var clay = 0
    var iron = 0
    var wheat = 0
    var total = 0
    var arrivalTime = ""
    list.foreach(r => {
      if(list.last == r) {
       arrivalTime = r.arrivalTime
      }
      units += r.units
      wood += r.bounty.wood
      clay += r.bounty.clay
      iron += r.bounty.iron
      wheat += r.bounty.wheat
      total += r.bounty.total

    })

    if (total != 0 && arrivalTime != "") {
      logger.info(s"Summary ${total}")

      ReturningUnit(
        units = units,
        bounty = Bounty(wood, clay, iron, wheat, total),
        arrivalTime = arrivalTime
      )
    } else {
      logger.info("Couldn't construct summary")
      ReturningUnit(
        units = units,
        bounty = Bounty(0, 0, 0, 0, 0),
        arrivalTime = arrivalTime
      )
    }
  }


}
