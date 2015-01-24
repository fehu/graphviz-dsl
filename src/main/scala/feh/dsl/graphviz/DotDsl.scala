package feh.dsl.graphviz

import feh.util.PrintIndents.Param

trait DotDsl extends GraphvizDsl with GraphvizDslDefaultWriter{

  override def write: DotWriter

  trait DotWriter extends DefaultWriter{
    def graphKey(implicit p: Param) = if(p.depth == 0) "digraph" else "subgraph"
  }

  implicit class DotElemWrapper(elem: Node){
    def -> (to: Node)   = Edge(elem, to)
    def -> (to: String) = Edge(elem, new ByNameElem(to))
  }

//  implicit class DotElemWrapper(elem: Elem){
//    def -> (to: Elem) = Edge(elem, to)
//    def -> (to: String) = Edge(elem, new ByNameElem(to))
//  }

  implicit class DotByNameElemWrapper(name: String){
    def -> (to: String) = Edge(new ByNameElem(name), new ByNameElem(to))
    def -> (to: Node) = Edge(new ByNameElem(name), to)
//    def -> (to: Elem) = Edge(new ByNameElem(name), to)
  }
}

class DotDslImpl(_defaultIndent: Int) extends DotDsl{
  val write = new DotWriter{
    def defaultIndent = _defaultIndent
  }

  val edgeWriter = new DefaultEdgeWriter {
    def edgeSymb = "->"
  }
}

object DotDsl{
  def withIndent(i: Int) = new DotDslImpl(i)

  object indent{
    lazy val _1 = new DotDslImpl(1)
    lazy val _2 = new DotDslImpl(2)
    lazy val _4 = new DotDslImpl(4)
  }
}