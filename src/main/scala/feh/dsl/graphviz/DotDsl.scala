package feh.dsl.graphviz

import feh.util.PrintIndents.Param
import scala.collection.mutable

trait DotDsl extends GraphvizDslDefaultWriter{

  override def write: DotWriter

  trait DotWriter extends DefaultWriter{
    def graphKey(implicit p: Param) = if(p.depth == 0) "digraph" else "subgraph"


    def chainEdges(edges: Seq[Edge]): Seq[AnyEdge] = {
      val buff = mutable.Buffer(edges.map(edge => LongEdge(edge.n1, edge.n2, edge.n1 :: edge.n2 :: Nil)): _*)

      def update(old1: LongEdge, old2: LongEdge, le: LongEdge){
        buff -= old1
        buff -= old2
        buff += le
      }

      import scala.util.control.Breaks._

      def doChaining: Unit =
        tryBreakable {
          for(edge <- buff) edge match {
            case x@LongEdge(start, end, all) =>
              buff.find(le => le.start == end).map{
                le => update(le, x, le.copy(start = start, all = all ++ le.all.drop(1)))
              } orElse buff.find(le => le.end == start).map{
                le => le.copy(end = end, all = le.all ++ all.drop(1))
              }
            case null =>
              break()
          }
        } catchBreak { doChaining }

      doChaining
      buff.toSeq
    }

  }

  implicit class DotElemWrapper(elem: Elem)(implicit wr: EdgeWriter){
    def -> (el: Elem) = Edge(elem, el)
  }
}

class DotDslImpl(_defaultIndent: Int) extends DotDsl{
  val write = new DotWriter{
    def defaultIndent = _defaultIndent
  }

  implicit val edgeWriter: EdgeWriter = new DefaultEdgeWriter {
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