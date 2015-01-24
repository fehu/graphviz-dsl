package feh.dsl.graphviz

import feh.util._
import feh.util.file._

object GraphvizExec extends ExecUtils{
  def graphviz(path: Path)(implicit format: OutFormat, prog: Prog) {
    val file = path.file
    val dir = Option(file.getParent) getOrElse "."
    val outName = path.splittedName._1 + "." + format.suffix
    graphviz(path, dir / outName)
  }

  def graphviz(in: Path, out: Path)(implicit format: OutFormat, prog: Prog){
    exec(prog.command, in.mkString(File.separator), "-T" + format.paramT, "-o", out.mkString(File.separator))
      .waitFor()
  }

  def readGraphviz(path: Path)(implicit format: OutFormat, prog: Prog): String = {
    val pr = redirectingStreams(exec(List(prog.command, "-T" + format.paramT), path))
    pr.waitFor()
    File.read[String](pr.getInputStream)
  }

  def writeAndExecGraphviz(path: Path, contents: String)(implicit format: OutFormat, prog: Prog){
    File(path).withOutputStream(File.write.utf8(contents))
    graphviz(path)
  }
}

case class OutFormat(paramT: String, suffix: String)

object OutFormat{
  object Svg extends OutFormat("svg", "svg")
  object Png extends OutFormat("png", "png")
}

case class Prog(command: String)

object Prog{
  object Dot extends Prog("dot")
  object Fdp extends Prog("fdp")
  object Neato extends Prog("neato")
  object Sfdp extends Prog("sfdp")
}

trait GraphvizInstallation