package example

import feh.util.Path._
import feh.util.file._

object RunExamples extends App{
  DotExamples.run()
}

object ExamplesConfig{
  
  def dirPath = `.` / "examples"
  
  lazy val dir = File(dirPath)
  
  if(!dir.exists) dir.mkdir()
}
