import java.time.{Instant, LocalDate}
import java.time.format.DateTimeFormatter

import scala.util.{Try, Success, Failure}
import scala.collection.immutable.ListMap

@main def run(args: String*): Unit =
  val result =
    for
      opts           <- Arguments(args*)
      _              <- if !opts.skipDownload then runGarminExport() else Success(())
      journalEntries <- Try(generateJournalEntries(opts))
      _              <- Try(writeJournalEntries(journalEntries, opts.destDir.get))
    yield journalEntries

  result match
    case Success(journalEntries) =>
      val dates = journalEntries.keySet
      if dates.isEmpty then scribe.info("Processed 0 files")
      else scribe.info(s"Processed ${dates.size} files between ${dates.min} and ${dates.max}")
    case Failure(err) =>
      throw err

// TODO: First downlaod the files (w/o --import flag), then validate / discard data, then run --import
def runGarminExport(): Try[os.CommandResult] =
  def runProcess(process: os.proc): Try[os.CommandResult] =
    scribe.info(s"Running ${process.commandChunks.mkString(" ")}")
    Try(process.call()).flatMap { res =>
      if res.exitCode == 0 then Success(res)
      else
        val msg = s"Unexpected exit code: ${res.exitCode}. Error: ${res.err.lines().mkString("\n")}"
        Failure(Exception(msg))
    }

  val cmdName        = "garmindb_cli.py"
  val checkExistsCmd = os.proc(Seq("which", cmdName))
  val downloadOpts = List(
    "--monitoring",
    "--activities",
    "--rhr",
    "--sleep",
    "--download",
    "--latest",
    "--import",
  )
  val downloadCmd = os.proc(cmdName :: downloadOpts)

  for
    _   <- runProcess(checkExistsCmd)
    res <- runProcess(downloadCmd)
  yield res

def generateJournalEntries(args: Arguments): ListMap[LocalDate, JournalEntry] =
  getUnprocessedDates(args)
    .flatMap { case (date, journalEntry) =>
      val year = date.getYear.toString
      val dailySummaryPath =
        args.sourceDir / "FitFiles" / "Monitoring" / year / s"daily_summary_$date.json"
      val dailySleepPath = args.sourceDir / "Sleep" / s"sleep_$date.json"

      val summaryAttrs =
        readGarminMetrics(dailySummaryPath, DailySummary.fromJson).getOrElse(Map.empty)
      val sleepAttrs  = readGarminMetrics(dailySleepPath, DailySleep.fromJson).getOrElse(Map.empty)
      val garminAttrs = summaryAttrs ++ sleepAttrs

      val mergedMeta      = ListMap((garminAttrs ++ journalEntry.meta).toSeq: _*)
      val newJournalEntry = JournalEntry(journalEntry.content, mergedMeta)

      if newJournalEntry.toMarkdown == journalEntry.toMarkdown && !args.overwrite then
        scribe.info(s"Data is the same for $date")
        None
      else Some(date -> JournalEntry(journalEntry.content, mergedMeta))
    }

def writeJournalEntries(journalEntries: ListMap[LocalDate, JournalEntry], path: os.Path): Unit =
  val tmpPath = os.Path(s"/tmp/journal-${Instant.now.getEpochSecond}")
  if !tmpPath.toIO.isDirectory() then tmpPath.toIO.mkdirs()

  journalEntries.foreach { case (date, journalEntry) =>
    val doc         = journalEntry.toMarkdown
    val tmpFilePath = tmpPath / s"${date.toString}.md"
    val actualPath  = path / s"${date.toString}.md"
    try
      os.write.over(tmpFilePath, doc)
      os.copy.over(tmpFilePath, actualPath)
      scribe.info(f"Wrote file to $actualPath")
    catch
      case e: Exception =>
        throw e
  }

def getUnprocessedDates(args: Arguments): ListMap[LocalDate, JournalEntry] =
  val fmt = DateTimeFormatter.ofPattern("yyyy-MM-dd")
  val journalFiles: Map[LocalDate, os.Path] = os
    .list(args.destDir.get)
    .filter(x => x.ext == "md")
    .flatMap { path =>
      val rawDate = path.baseName.split("-").take(3).mkString("-")
      Try(LocalDate.parse(rawDate, fmt)).toOption.map(date => date -> path)
    }
    .toMap

  if journalFiles.isEmpty then return ListMap.empty[LocalDate, JournalEntry]

  val journalDates   = journalFiles.keySet
  val maxExportDate  = getExportMaxDate(args.sourceDir)
  val maxJournalDate = journalDates.max
  val minJournalDate = journalDates.min
  val cuttoffDate    = args.dateMin.getOrElse(journalDates.min)

  val dateMin = if minJournalDate.isBefore(cuttoffDate) then cuttoffDate else minJournalDate
  val dateMax = if maxJournalDate.isBefore(maxExportDate) then maxExportDate else maxJournalDate

  val dateJournalPairs = dateRange(dateMin, dateMax)
    .flatMap { date =>
      journalFiles.get(date) match
        case Some(path) =>
          val journalEntry    = JournalEntry.getOrCreate(path)
          val (meta, content) = (journalEntry.meta, journalEntry.content)
          Some(date -> journalEntry)
        case None =>
          Some(date -> JournalEntry())
    }
  ListMap(dateJournalPairs: _*)

def getExportMaxDate(src: os.Path): LocalDate =
  val year             = LocalDate.now().getYear.toString
  val dailySummaryPath = src / "FitFiles" / "Monitoring" / year
  val dailySleepPath   = src / "Sleep"
  val fmt              = DateTimeFormatter.ofPattern("yyyy-MM-dd")

  def tryParseDate(path: os.Path, prefix: String): Option[LocalDate] =
    os.list(path)
      .filter(x => x.baseName.startsWith(prefix) && x.ext == "json" && os.stat(x).size > 0)
      .lastOption
      .flatMap(x => x.baseName.split(prefix).lastOption.map(LocalDate.parse(_, fmt)))

  val yesterday = LocalDate.now().minusDays(1)
  val maxDate = List(
    tryParseDate(src / "FitFiles" / "Monitoring" / year, "daily_summary_"),
    tryParseDate(src / "Sleep", "sleep_"),
    tryParseDate(src / "RHR", "rhr_")
  ).flatten.max

  if maxDate.isAfter(yesterday) then yesterday else maxDate

def readGarminMetrics(
    path: os.Path,
    parser: String => Either[Exception, GarminMetrics]
): Either[Exception, ListMap[String, Any]] =
  if path.toIO.exists() then parser(os.read(path)).map(_.frontMatter)
  else Left(java.io.FileNotFoundException(path.toString))

def dateRange(
    startDate: LocalDate,
    endDate: LocalDate,
    step: Long = 1,
    acc: List[LocalDate] = List.empty[LocalDate]
): List[LocalDate] =
  if startDate isAfter endDate then acc.reverse
  else dateRange(startDate.plusDays(step), endDate, step, startDate :: acc)
