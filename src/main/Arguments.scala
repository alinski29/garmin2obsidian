import scopt.*
import scopt.OParser
import scala.util.{Try, Success, Failure}
import java.io.FileNotFoundException
import java.time.LocalDate
import java.time.format.DateTimeFormatter

case class Arguments(
    sourceDir: os.Path = os.home / "HealthData",
    destDir: Option[os.Path] = None,
    exportConfigPath: Option[os.Path] = Some(
      os.RelPath(".GarminDb/GarminConnectConfig.json").resolveFrom(os.home)
    ),
    dateMin: Option[LocalDate] = None,
    skipDownload: Boolean = false,
    overwrite: Boolean = false,
)

object Arguments:
  def apply(args: String*): Try[Arguments] =
    OParser.parse(parser, args, Arguments()) match
      case Some(opts) =>
        if !opts.sourceDir.toIO.isDirectory() then
          Failure(new Exception(s"${opts.sourceDir} is not a directory"))
        if !opts.exportConfigPath.get.toIO.isFile() then
          Failure(
            new FileNotFoundException(
              s"Missing export configuration file at ${opts.exportConfigPath}"
            )
          )
        else Success(opts)
      case _ =>
        Failure(new IllegalArgumentException("Wrong arguments"))

  private val builder = OParser.builder[Arguments]
  private val fmt     = DateTimeFormatter.ofPattern("yyyy-MM-dd")
  private val parser = {
    import builder.*
    OParser.sequence(
      programName("Garmin to Obisidian converter"),
      opt[String]("source-dir")
        .action((x, c) => c.copy(sourceDir = os.Path(x)))
        .text("Path to Garmin data"),
      opt[String]("config-path")
        .action((x, c) => c.copy(exportConfigPath = Some(os.Path(x))))
        .text("Path to configuration for garmindb_cli.py command"),
      opt[String]("dest-dir")
        .required()
        .action((x, c) => c.copy(destDir = Some(os.Path(x))))
        .text("Path to directory with daily journal notes in markdown format"),
      opt[String]("date-min")
        .action((x, c) => c.copy(dateMin = Try(LocalDate.parse(x, fmt)).toOption))
        .text("Minimum date to use for journal notes"),
      opt[Unit]("skip-download")
        .action((x, c) => c.copy(skipDownload = true))
        .text("Skiping downloading files from garmin"),
      opt[Unit]("overwrite")
        .action((x, c) => c.copy(overwrite = true))
        .text("Will overwrite journal files even when content is unchanged")
    )
  }
