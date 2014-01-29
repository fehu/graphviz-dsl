package feh.dsl.graphviz

import feh.util.PrintIndents

trait FdpDsl extends GraphvizDsl with GraphvizDslDefaultWriter{

  override def write: FdpWriter

  trait FdpWriter extends DefaultWriter{
    def graphKey(implicit p: PrintIndents.Param) = if(p.depth == 0) "graph" else "subgraph"
  }


  implicit class FdpElemWWrapper(el: Elem){
    def --(other: Elem) = Edge(el, other)
  }
}

class FdpDslImpl(_defaultIndent: Int) extends FdpDsl{
  override val write = new FdpWriter {
    def defaultIndent = _defaultIndent
  }

  val edgeWriter = new DefaultEdgeWriter {
    def edgeSymb = "--"
  }
}

object FdpDsl{
  object indent{
    lazy val _1 = new FdpDslImpl(1)
    lazy val _2 = new FdpDslImpl(2)
    lazy val _4 = new FdpDslImpl(4)
  }
}