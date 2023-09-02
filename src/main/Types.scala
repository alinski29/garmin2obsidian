import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._

import java.time.{Instant, LocalDate, LocalDateTime, ZoneId, ZoneOffset}
import java.time.format.DateTimeFormatter
import scala.collection.immutable.ListMap

final case class JournalEntry(content: String = "", meta: ListMap[String, Any] = ListMap.empty):
  def toMarkdown: String =
    val yamlHeader =
      if meta.nonEmpty then "---\n" + serializeYaml(meta) + "\n" + "---\n"
      else ""

    Seq(yamlHeader, content).mkString("\n")

object JournalEntry:
  def getOrCreate(path: os.Path): JournalEntry =
    if path.toIO.exists() then parseJournalContent(os.read.lines(path))
    else JournalEntry()

  private def parseJournalContent(lines: IndexedSeq[String]): JournalEntry =
    val linesZipped = lines.zipWithIndex
    val positions = for
      startHeader <- linesZipped.find { case (line, idx) => line == "---" }.map(_._2 + 1)
      endHeader <- linesZipped
        .drop(startHeader)
        .find { case (line, idx) => line == "---" }
        .map(_._2 + 1)
    yield (startHeader, endHeader)
    positions match
      case Some((startIdx, endIdx)) =>
        val pairs = lines
          .slice(startIdx, endIdx - 1)
          .filter(_.contains(":"))
          .map { x =>
            val splits       = x.split(":", 2)
            val (key, value) = (splits.head.trim, splits.last.trim)
            key -> value
          }
        val meta    = ListMap(pairs: _*)
        val content = lines.drop(endIdx).filterNot(_.isBlank) mkString ("\n")
        JournalEntry(content, meta)
      case _ =>
        // no metadata
        JournalEntry(lines.mkString("\n"))

trait GarminMetrics:
  def frontMatter: ListMap[String, Any]

  def dayOfWeek(
      rawDate: String,
      fmt: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
  ): String =
    LocalDate.parse(rawDate, fmt).getDayOfWeek.toString.toLowerCase.capitalize

final case class DailySummary(
    calendarDate: String,
    totalSteps: Option[Int],
    activeSeconds: Option[Int],
    totalDistanceMeters: Option[Int],
    sleepingSeconds: Option[Int],
    restingHeartRate: Option[Int],
    minHeartRate: Option[Int],
    maxHeartRate: Option[Int],
    totalKilocalories: Option[Int],
    activeKilocalories: Option[Int],
    floorsAscended: Option[Double],
    floorsDescended: Option[Double]
) extends GarminMetrics:

  def frontMatter = ListMap(
    "day_of_week"         -> dayOfWeek(calendarDate),
    "total_steps"         -> totalSteps,
    "resting_heart_rate"  -> restingHeartRate,
    "min_heart_rate"      -> minHeartRate,
    "max_heart_rate"      -> maxHeartRate,
    "active_seconds"      -> activeSeconds,
    "total_kilocalories"  -> totalKilocalories,
    "active_kilocalories" -> activeKilocalories,
    "floors_ascended"     -> floorsAscended,
    "floors_descended"    -> floorsDescended
  )

object DailySummary:
  private given dailySummaryDecoder: Decoder[DailySummary] = deriveDecoder

  def fromJson(input: String): Either[io.circe.Error, DailySummary] =
    parser.decode[DailySummary](input)

final case class DailySleep(
    calendarDate: String,
    sleepTimeSeconds: Int,
    deepSleepSeconds: Option[Int],
    awakeSleepSeconds: Option[Int],
    sleepStartTimestampGMT: Option[Instant],
    sleepStartTimestampLocal: Option[Instant],
    sleepEndTimestampGMT: Option[Instant],
    sleepEndTimestampLocal: Option[Instant]
) extends GarminMetrics:

  def frontMatter = ListMap(
    "day_of_week"          -> dayOfWeek(calendarDate),
    "sleep_seconds"        -> sleepTimeSeconds,
    "deep_sleep_seconds"   -> deepSleepSeconds,
    "awake_sleep_seconds"  -> awakeSleepSeconds,
    "sleep_start_time"     -> sleepStartTimestampLocal,
    "sleep_end_time"       -> sleepEndTimestampLocal,
    "sleep_start_time_utc" -> sleepStartTimestampGMT,
    "sleep_end_time_utc"   -> sleepEndTimestampGMT
  )

object DailySleep:

  def fromJson(input: String): Either[io.circe.Error, DailySleep] =
    parser.decode[DailySleep](input)

  private given dailySleepDecoder: Decoder[DailySleep] = new Decoder[DailySleep]:
    final def apply(c: HCursor): Decoder.Result[DailySleep] =
      val sleep = c.downField("dailySleepDTO")
      for
        date         <- sleep.get[String]("calendarDate")
        sleepSeconds <- sleep.get[Int]("sleepTimeSeconds")
      yield DailySleep(
        calendarDate = date,
        sleepTimeSeconds = sleepSeconds,
        deepSleepSeconds = sleep.get[Int]("deepSleepSeconds").toOption,
        awakeSleepSeconds = sleep.get[Int]("awakeSleepSeconds").toOption,
        sleepStartTimestampGMT =
          sleep.get[Long]("sleepStartTimestampGMT").map(Instant.ofEpochMilli).toOption,
        sleepStartTimestampLocal =
          sleep.get[Long]("sleepStartTimestampLocal").map(Instant.ofEpochMilli).toOption,
        sleepEndTimestampGMT =
          sleep.get[Long]("sleepEndTimestampGMT").toOption.map(Instant.ofEpochMilli),
        sleepEndTimestampLocal =
          sleep.get[Long]("sleepEndTimestampLocal").toOption.map(Instant.ofEpochMilli)
      )

def serializeYaml(data: Map[String, Any]): String =
  data
    .map { case (key, maybeValue) =>
      val value = maybeValue match {
        case Some(ts: Instant) =>
          ts.toString
        // val datetime = LocalDateTime.ofInstant(ts, ZoneOffset.UTC)
        // datetime.format(new DateTimeFormatter.ofPattern("yyyy-MM-dd HH:MM:SS))
        case Some(x) => x.toString
        case None    => ""
        case x       => x.toString
      }
      key -> value
    }
    .filterNot { case (k, v) => v.isBlank }
    .map { case (k, v) => s"$k: $v" }
    .mkString("\n")
