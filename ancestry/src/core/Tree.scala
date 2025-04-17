package com.scilari.ancestry.core

import scala.collection.immutable.ArraySeq
import com.scilari.ancestry.AncestryTree
import scala.annotation.tailrec

sealed trait Tree[T] extends NextGenerationProducer[T] {

  /** Payload
    */
  val data: T

  /** Weight of the data. This is used to calculate the weight of the tree (sum of all leaves).
    */
  var weight: Double = 0.0

  /** Height of the tree. Distance to the furthest leaf.
    *
    * @return
    */
  def height: Int

  /** Level (depth) of the tree. Distance to the root node.
    *
    * @return
    */
  def level: Int = parent match {
    case None    => 0
    case Some(p) => 1 + p.level
  }

  /** Recursive update of leaf weigths
    *
    * @param dataWeight
    * @return
    */
  def updateWeight(dataWeight: T => Double): Double

  /** Size of the tree. Number of nodes in the tree.
    *
    * @return
    */
  def size = nodes.size

  /** Returns all nodes in the tree. This includes the root node, all branches and leaves.
    *
    * @return
    */
  def nodes: List[Tree[T]]

  /** Number of nodes in the tree.
    *
    * @return
    */
  def nodeCount: Int

  /** Returns all leaves in the tree.
    *
    * @return
    */
  def leaves: List[Leaf[T]] = leavesAcc(Nil)

  /** All leaves in the tree. This is a cached version of the leaves.
    */
  lazy val leavesCached: ArraySeq[Leaf[T]] = ArraySeq(leavesAcc(Nil)*)

  /** Number of leaves in the tree.
    *
    * @return
    */
  def leafCount: Int

  /** Ancestors of the current node. This includes the current node and all parents up to the root
    * (inclusive).
    *
    * @return
    */
  def ancestors: List[Tree[T]] = this :: parent.map { _.ancestors }.getOrElse(Nil)

  /** The root node of the tree.
    *
    * @return
    */
  @tailrec
  final def root: Tree[T] = {
    parent match {
      case None    => this
      case Some(p) => p.root
    }
  }

  /** True if the node is the root of the tree.
    *
    * @return
    */
  def isRoot: Boolean = parent.isEmpty

  /** True if the node is a leaf.
    *
    * @return
    */
  def isLeaf: Boolean = this match {
    case _: Leaf[T] => true
    case _          => false
  }

  /** True if the node is a branch.
    *
    * @return
    */
  def isBranch: Boolean = !isLeaf

  private[ancestry] var parent: Option[Branch[T]] = None

  private[core] def leavesAcc(acc: List[Leaf[T]]): List[Leaf[T]]

}

final case class Branch[T](
    data: T,
    children: List[Tree[T]]
) extends Tree[T] {
  require(children.nonEmpty, "Branch needs children")

  // Rewire parents
  children.foreach { _.parent = Some(this) }

  def this(data: T, child: Tree[T]) = this(data, List(child))

  def height: Int = 1 + children.map { _.height }.max
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

  def height: Int = 0
  def nodes = List(this)
  def nodeCount: Int = 1

  def leavesAcc(acc: List[Leaf[T]]) = this :: acc
  def leafCount: Int = 1

  def updateWeight(weigthFunction: T => Double): Double = {
    weight = weigthFunction(data)
    weight
  }

}
