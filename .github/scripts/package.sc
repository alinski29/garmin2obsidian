//> using scala "3.1.2"
//> using lib "com.lihaoyi::os-lib:0.8.0"
import scala.util.Properties

val platformSuffix: String = {
  val os =
    if (Properties.isWin) "pc-win32"
    else if (Properties.isLinux) "pc-linux"
    else if (Properties.isMac) "apple-darwin"
    else sys.error(s"Unrecognized OS: ${sys.props("os.name")}")
  os
}
val artifactsPath = os.Path("artifacts", os.pwd)
val appName = "garmin2obsidian"
val destPath =
  if (Properties.isWin) artifactsPath / s"$appName-$platformSuffix.exe"
  else artifactsPath / s"$appName-$platformSuffix"
val scalaCLILauncher =
  if (Properties.isWin) "scala-cli.bat" else "scala-cli"

os.makeDir(artifactsPath)
os.proc(scalaCLILauncher,"--power",  "package", ".", "-o", destPath, "--assembly")
  .call(cwd = os.pwd)
  .out
  .text()
  .trim
