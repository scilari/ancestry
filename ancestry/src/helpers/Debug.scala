package com.scilari.ancestry.helpers

import com.scilari.ancestry.core.{Branch, Leaf, Tree}

object Debug {
  def debugString[T](
      tree: Tree[T],
      toString: Tree[T] => String = (t: Tree[T]) => t.data.toString
  ): String = {
    val INTENT_PER_LEVEL = 4
    def rec(tree: Tree[T], intendation: Int): String = {
      val prefix =
        if (intendation > 0)
          " " * (intendation - INTENT_PER_LEVEL) + "\\" + "." * (INTENT_PER_LEVEL - 1)
        else ""
      val tag =
        prefix + toString(tree) + s" (${tree.parent.map { p => toString(p) }.getOrElse("root")})" + "\n"
      tree match {
        case _: Leaf[T] => tag
        case b: Branch[T] =>
          tag + b.children.map(c => rec(c, intendation + INTENT_PER_LEVEL)).fold("")(_ + _)
      }
    }

    val info = s"Tree: nodes = ${tree.nodeCount} (${tree.leafCount}), depth = ${tree.height}"
    val recRepresentation = rec(tree, 0)
    info + "\n" + recRepresentation
  }
}
