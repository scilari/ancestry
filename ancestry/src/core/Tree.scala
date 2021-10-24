package com.scilari.ancestry.core

import scala.collection.immutable.ArraySeq

sealed trait Tree[T] extends NextGenerationProducer[T] with Iterable[Tree[T]] {

  val data: T
  var weight: Double = 0.0

  override def iterator: Iterator[Tree[T]] = nodes.iterator

  def depth: Int
  def level: Int = parent match {
    case None    => 0
    case Some(p) => 1 + p.level
  }

  def updateWeight(dataWeight: T => Double): Double

  def nodes: Seq[Tree[T]]
  def nodeCount: Int
  def leafCount: Int

  def ancestors: List[Tree[T]] = this :: parent.map { _.ancestors }.getOrElse(Nil)

  private[ancestry] var parent: Option[Branch[T]] = None

  def isRoot: Boolean = parent.isEmpty

}

case class Branch[T](
    data: T,
    children: Seq[Tree[T]]
) extends Tree[T] {
  require(children.nonEmpty, "Branch needs children")

  // Rewire parents
  children.foreach { _.parent = Some(this) }

  def this(data: T, child: Tree[T]) = this(data, Seq(child))

  def depth: Int = 1 + children.map { _.depth }.max
  def updateWeight(dataWeight: T => Double): Double = {
    weight = children.map {_.updateWeight(dataWeight)}.sum
    weight
  }
  def leafCount: Int = children.map { _.leafCount }.sum
  def nodeCount: Int = 1 + children.map { _.nodeCount }.sum
  def nodes: Seq[Tree[T]] = this +: children.flatMap(_.nodes)

}

case class Leaf[T](
    data: T
) extends Tree[T] {

  def depth: Int = 1
  def leafCount: Int = 1
  def nodes = Seq(this)
  def nodeCount: Int = 1

  def updateWeight(weigthFunction: T => Double): Double = { 
    weight = weigthFunction(data)
    weight
  }

}
