package example

import feh.dsl.graphviz.{Prog, OutFormat, GraphvizExec, DotDslImpl}
import GraphvizExec._

object DotExamples {
  val examples = DotExample1 :: DotExample2 :: Nil

  def run() = examples.foreach(_.run())
}

abstract class DotExample extends DotDslImpl(4){
  def run()

  def runExample(name: String, graph: Graph): Unit ={
    val dot = write.graph(graph)
//    path.file.withOutputStream(File.write.utf8(dot))
    val dotFile = ExamplesConfig.dirPath / (name + ".dot")
    writeAndExecGraphviz(dotFile, dot)(OutFormat.Png, Prog.Dot)
  }
  
  
}

object DotExample1 extends DotExample{
  def run() = runExample("example-1",
    Graph("Test 1")(
      Node("Node 1"),
      "Node 2" :| attr.Label("Test"),
      "Node 3".node,
      "Node 1" -> "Node 2",
      "Node 2" -> "Node 3"
    )
  ) 
}

object DotExample2 extends DotExample{
  import Attributes._

  def run() = runExample("example-2",
    Cluster("Test 2", Label("Clusters example"))(
      Cluster("Sub 1", autoLabel)(
        "Node 1" -> "Node 2",
        "Node 1" -> "Node 3"
      ),
      Graph("Sub 2", cluster, autoLabel, fill)(
        "Node 4" -> "Node 5",
        "Node 5" -> "Node 3"
      )
    )
  )
}
