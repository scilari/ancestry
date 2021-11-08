package com.scilari.ancestry.core

import scala.collection.immutable.ArraySeq
import com.scilari.ancestry.AncestryTree

sealed trait Tree[T] extends NextGenerationProducer[T] with Iterable[Tree[T]] {

  // Payload
  val data: T

  // Combined weight of this subtree (updated by updateWeight)
  var weight: Double = 0.0

  override def iterator: Iterator[Tree[T]] = nodes.iterator

  // Distance to the furthers leaf
  def depth: Int

  // Distance to the root node
  def level: Int = parent match {
    case None    => 0
    case Some(p) => 1 + p.level
  }

  // Recursively updates subtrees based on data weighing
  def updateWeight(dataWeight: T => Double): Double

  def nodes: List[Tree[T]]
  def nodeCount: Int
  def leaves: List[Leaf[T]] = leavesAcc(Nil)
  lazy val leavesCached: ArraySeq[Leaf[T]] = ArraySeq(leavesAcc(Nil)*)
  def leafCount: Int

  private[core] def leavesAcc(acc: List[Leaf[T]]): List[Leaf[T]]

  // Ancestors starting from current node and ending to the root
  def ancestors: List[Tree[T]] = this :: parent.map { _.ancestors }.getOrElse(Nil)

  private[ancestry] var parent: Option[Branch[T]] = None

  def isRoot: Boolean = parent.isEmpty

}

final case class Branch[T](
    data: T,
    children: List[Tree[T]]
) extends Tree[T] {
  require(children.nonEmpty, "Branch needs children")

  // Rewire parents
  children.foreach { _.parent = Some(this) }

  def this(data: T, child: Tree[T]) = this(data, List(child))

  def depth: Int = 1 + children.map { _.depth }.max
  def updateWeight(dataWeight: T => Double): Double = {
    weight = children.map { _.updateWeight(dataWeight) }.sum
    weight
  }

  def nodes: List[Tree[T]] = this :: children.flatMap(_.nodes)
  def nodeCount: Int = 1 + children.map { _.nodeCount }.sum

  def leavesAcc(acc: List[Leaf[T]]) = {
    var acc2 = acc
    children.foreach { c => acc2 = c.leavesAcc(acc2) }
    acc2
  }
  def leafCount: Int = children.map { _.leafCount }.sum

}

final case class Leaf[T](
    data: T
) extends Tree[T] {

  def depth: Int = 1
  def nodes = List(this)
  def nodeCount: Int = 1

  def leavesAcc(acc: List[Leaf[T]]) = this :: acc
  def leafCount: Int = 1

  def updateWeight(weigthFunction: T => Double): Double = {
    weight = weigthFunction(data)
    weight
  }

}
