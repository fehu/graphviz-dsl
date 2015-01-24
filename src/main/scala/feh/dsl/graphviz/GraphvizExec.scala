package feh.dsl.graphviz

import feh.util._
import feh.util.file._

trait GraphvizExec extends ExecUtils{
  def graphvizDirPath: Option[Path] = None

  def execGraphviz(path: Path)(implicit format: OutFormat, prog: Prog) {
    execGraphviz(path, path.file.name.dropRight(path.ext.length |> (l => if(l==0) 0 else l+1)))
  }

  def execGraphviz(path: Path, name: String)(implicit format: OutFormat, prog: Prog) {
    val dir = Option(path.file.getParent) getOrElse "."
    execGraphviz(path, dir / (name + format.suffix)/*Path(dir + File.separator + name + "." + format.suffix, )*/)
  }

  def execGraphviz(in: Path, out: Path)(implicit format: OutFormat, prog: Prog){
    exec(List(graphvizDirPath.map(_.toString + separator).getOrElse("") + prog.command, "-T" + format.graphvizTparam, s"-o$out"), in)
//    exec(graphvizDirPath.map(_.toString + separator).getOrElse("") + prog.command, "-T" + format.graphvizTparam, s"-o$out", in)
//      .waitFor()
  }

  def readGraphviz(path: Path)(implicit format: OutFormat, prog: Prog): String = {
    val pr = redirectingStreams(exec(List(graphvizDirPath.map(_.toString + separator).getOrElse("") + prog.command, "-T" + format.graphvizTparam), path))
    pr.waitFor()
    File.read[String](pr.getInputStream)
  }

  def writeAndExec(path: Path, contents: String)(implicit format: OutFormat, prog: Prog){
    File(path).withOutputStream(File.write.utf8(contents))
    execGraphviz(path)
  }
}

trait OutFormat{
  def graphvizTparam: String
  def suffix: String
}

object OutFormat{
  object Svg extends OutFormat{
    def graphvizTparam = "svg"
    def suffix = "svg"
  }
}

trait Prog{
  def command: String
}

object Prog{
  object Dot extends Prog{ def command = "dot" }
  object Fdp extends Prog{ def command = "fdp" }
  object Neato extends Prog{ def command = "neato" }
  object Sfdp extends Prog{ def command = "sfdp" }
}

trait GraphvizInstallation