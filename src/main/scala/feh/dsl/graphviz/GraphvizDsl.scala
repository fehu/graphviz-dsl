package feh.dsl.graphviz

import feh.util._
import feh.util.PrintIndents._
import scala.collection.mutable

trait GraphvizDsl{
  trait Elem{
    /** one-line value
      */
    def value: String
    def printValue: String
    def attrs: Set[_ <: Attribute[_]]

    override def toString = printValue

    def isGraph = false
    def isNode = false
    def isEdge = false
    def isAttr = false

    def notGraph = !isGraph
    def notNode = !isNode
    def notEdge = !isEdge
    def notAttr = !isAttr
  }

  trait Attribute[T]{
    def key: String
    def value: T
    def stringValue: T => Option[String]

    override def equals(obj: scala.Any): Boolean = PartialFunction.cond(obj) {
      case attr: Attribute[_] => key == attr.key
    }

    override def toString: String = stringValue(value).map(s => s"$key=$s") getOrElse ""
  }

  trait GraphAttr[T] extends Attribute[T]
  trait NodeAttr[T] extends Attribute[T]
  trait EdgeAttr[T] extends Attribute[T]

  abstract class AttrImpl[T](val key: String, val value: T, val stringValue: T => Option[String]) extends Attribute[T]

  trait ScopeAttr[A <: Attribute[_]] extends Elem{
    def attrs: Set[A]
    def tpe: String

    def value = if(attrs.isEmpty) "" else attrs.mkString(s"$tpe [ ", "; ", " ]")

    def printValue = value

    override def isAttr = true
  }
  case class ScopeGraphAttr[T](attrs: Set[GraphAttr[T]])  extends ScopeAttr[GraphAttr[T]]{ def tpe = "graph"}
  case class ScopeNodeAttr[T](attrs: Set[NodeAttr[T]])    extends ScopeAttr[NodeAttr[T]]{ def tpe = "node"}
  case class ScopeEdgeAttr[T](attrs: Set[EdgeAttr[T]])    extends ScopeAttr[EdgeAttr[T]]{ def tpe = "edge"}

  object Attributes{
    abstract class StringAttr(key: String, value: String) extends AttrImpl[String](key, value, s => Option(s).map(_.quoted))
    abstract class IntAttr(key: String, value: Int) extends AttrImpl[Int](key, value, _.toString |> Some.apply)

    case class Id(id: String) extends StringAttr("id", id) with NodeAttr[String] with EdgeAttr[String] with GraphAttr[String]
    case class Label(text: String) extends StringAttr("label", text) with NodeAttr[String] with EdgeAttr[String] with GraphAttr[String]
    case class Tooltip(text: String) extends StringAttr("tooltip", text) with NodeAttr[String] with EdgeAttr[String] with GraphAttr[String]
    case class Font(name: String) extends StringAttr("fontname", name) with NodeAttr[String] with EdgeAttr[String] with GraphAttr[String]
    case class FontSize(size: Int) extends IntAttr("fontsize", size) with NodeAttr[Int] with EdgeAttr[Int] with GraphAttr[Int]
    case class Color(color: java.awt.Color) extends AttrImpl[java.awt.Color]("color", color, Option(_) map (_.hexRGB.quoted))
      with NodeAttr[java.awt.Color] with EdgeAttr[java.awt.Color] with GraphAttr[java.awt.Color]

    case object Fill extends StringAttr("style", "filled") with NodeAttr[String]
    // todo
  }

  case class Node(name: String, attrs: Set[NodeAttr[_]] = Set()) extends Elem{
    def value: String = name + (
      if(attrs.isEmpty) ""
      else attrs.mkString(" [", " ", "]")
      )

    def printValue = name

    override def isNode = true
  }

  trait AnyEdge extends Elem

  case class Edge(n1: Elem, n2: Elem, attrs: Set[EdgeAttr[_]] = Set()) extends AnyEdge{

    override def isEdge = true

    def value = edgeWriter.write(this)

    def printValue = value
  }

  protected case class LongEdge(start: Elem, end: Elem, all: Seq[Elem]) extends AnyEdge{
    def value = edgeWriter.write(this)

    def attrs = Set()

    def printValue = value
  }

  trait EdgeWriter{
    def write(edge: AnyEdge): String
  }

  def edgeWriter: EdgeWriter

  case class Graph(name: String, elems: Seq[Elem], attrs: Set[GraphAttr[_]] = Set()) extends Elem{
    def value: String = ??? // shouldn't be used as graph is multi-line
    def printValue = name
    override def isGraph = true
  }

  trait Root extends Graph{
    override def value = write.root(this).dropRight(1)
    override def toString = value
  }

  object Root{
    def apply(name: String, elems: Seq[Elem], attrs: Set[GraphAttr[_]] = Set())
             (implicit p: Param = null) = new Graph(name, elems, attrs) with Root
  }

  trait Writer{
    def defaultIndent: Int

    def root(gr: Root): String
    def graph(gr: Graph)(implicit p: Param)
  }

  def write: Writer
}

trait GraphvizDslDefaultWriter{
  self: GraphvizDsl =>

  override def write: DefaultWriter

  trait DefaultWriter extends Writer{
    def graphKey(implicit p: Param): String

    def attrsOneLine(elem: Elem): String = elem match {
      case Node(_, attrs) => "node " + attrsOneLine(attrs)
    }

    def attrsOneLine(attrs: Set[_ <: Attribute[_]]): String =
      if(attrs.isEmpty) "" else attrs.mkString("[ " + "; " + " ]").filterNot('\n' ==)

    def surroundGraph(gr: Graph)(body: => Unit)(implicit p: Param){
      printlni(s"$graphKey ${gr.name} {")
      printlni("")
      nextDepth(body)
      printlni("}")
    }

    def graph(gr: Graph)(implicit p: Param){
      surroundGraph(gr){
        nodes(gr)
        edges(gr)
        gr.elems.collect{
          case g: Graph => graph(g)
        }
      }
    }

    def nodes(gr: Graph)(implicit p: Param){
      def extractEdgeNodes: Edge => Set[Node] = {
        case Edge(n1: Node, n2: Node, _) => Set(n1, n2)
        case Edge(e: Edge, n: Node, _) => extractEdgeNodes(e) + n
        case Edge(n: Node, e: Edge, _) => extractEdgeNodes(e) + n
        case _ => Set()
      }

      val (attrLess, withAttrs) = gr.elems.collect{
        case n: Node => Set(n)
        case e: Edge => extractEdgeNodes(e)
      }.flatten.toSet.span(_.attrs.isEmpty)

      if(attrLess.nonEmpty) printlni(attrLess.mkString("", "; ", "\n"))
      if(withAttrs.nonEmpty) withAttrs.map(_.value) foreach printlni
    }

    def maxChainLength = 20
    
    def chainEdges(edges: Seq[Edge]): Seq[AnyEdge] = edges.toList /*{
      val buff = mutable.Buffer(edges.map(edge => LongEdge(edge.n1, edge.n2, edge.n1 :: edge.n2 :: Nil)): _*)

      def update(old1: LongEdge, old2: LongEdge, le: LongEdge){
        buff -= old1
        buff -= old2
        buff += le
      }

      import scala.util.control.Breaks._

      def fbuff = buff.filter(_.all.length != maxChainLength)
      
      def doChaining: Unit =
        tryBreakable {
          for(edge <- fbuff) edge match {
            case LongEdge(_, _, all) if all.length == maxChainLength => // nothing
            case x@LongEdge(start, end, all) =>
              fbuff.find(le => le.start == end).map{
                le => update(le, x, le.copy(start = start, all = all ++ le.all.drop(1)))
              } orElse fbuff.find(le => le.end == start).map{
                le => le.copy(end = end, all = le.all ++ all.drop(1))
              }
            case null =>
              break()
          }
        } catchBreak { doChaining }

      doChaining
      buff.toSeq
    }
*/

    def edgeSeq(edge: Edge): Seq[Edge] = edge match{
      case edg@Edge(e1: Edge, e2: Edge, _) => edgeSeq(e1) ++ Seq(edg) ++ edgeSeq(e2)
      case edg@Edge(e: Edge, _, _) => edg +: edgeSeq(e)
      case edg@Edge(_, e: Edge, _) => edgeSeq(e) :+ edg
      case edg: Edge => Seq(edg)
      case _ => Nil
    }

    def edges(gr: Graph)(implicit p: Param){
      val edges = gr.elems.collect{case e: Edge => e}.flatMap(edgeSeq)

      val (noAttrs, withAttrs) = edges.span(_.attrs.isEmpty)
      val chained = chainEdges(noAttrs)
      chained map {e => printlni(e.value + ";")}
      if(withAttrs.nonEmpty) printlni(withAttrs.map(_.value).mkString("", ";\n", ";"))
    }

    def root(gr: Root) = {
      implicit val b = newBuilder(defaultIndent)
      graph(gr)
      b.mkString
    }
  }

  trait DefaultEdgeWriter extends EdgeWriter{
    def edgeSymb: String

    def write(edge: AnyEdge) = edge match{
      case LongEdge(_, _, all) => all.map(elemToStr).mkString(s" $edgeSymb ")
      case Edge(n1, n2, _) => elemToStr(n1) + " " + edgeSymb + " " + elemToStr(n2)
    }

    protected def elemToStr(elem: Elem): String = elem match{
      case Node(name, _) => name
      case Graph(name, _, _) => name
      case e: Edge => e.value
    }
  }
}
