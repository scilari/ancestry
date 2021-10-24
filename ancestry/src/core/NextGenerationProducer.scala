package com.scilari.ancestry.core

trait NextGenerationProducer[T] {

  /** Produces an ancestry tree representing the next generation.
    *
    * @param breed
    *   Defines the offspring for the data in the leaves. Returning an empty Seq here corresponds to the bloodline being
    *   dead.
    * @param merge
    *   Defines how the data should be combined when only child is merged to the parent.
    * @return
    *   The next generation tree.
    */
  def nextGeneration(
      breed: T => Seq[T],
      merge: (T, T) => T
  ): Option[Tree[T]] = {

    this match {
      case leaf: Leaf[T] => {
        val childData = breed(leaf.data)
        childData.size match {
          case 0 => None // Bloodline is dead
          case 1 => Some(Leaf(data = merge(leaf.data, childData.head)))
          case _ => Some(Branch(leaf.data, childData.map(d => Leaf(d))))
        }
      }

      case branch: Branch[T] => {
        val nextGenChildren = branch.children.flatMap(_.nextGeneration(breed, merge))
        nextGenChildren.size match {
          case 0 => None
          case 1 => mergeOnlyChildWithParent(branch, nextGenChildren.head, merge)
          case _ => Some(branch.copy(children = nextGenChildren))
        }
      }
    }
  }

  private def mergeOnlyChildWithParent(parent: Tree[T], child: Tree[T], merge: (T, T) => T): Option[Tree[T]] = {
    val mergedData = merge(parent.data, child.data)
    child match {
      case childLeaf: Leaf[T] => Some(Leaf[T](mergedData)) // This just becomes the leaf
      // Granny adopts the grandchildren
      case childBranch: Branch[T] => Some(Branch[T](mergedData, childBranch.children))
    }
  }

}
