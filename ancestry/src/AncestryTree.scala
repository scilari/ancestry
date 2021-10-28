package com.scilari.ancestry

import com.scilari.ancestry.core.Tree
import com.scilari.ancestry.core.Branch
import com.scilari.ancestry.core.Leaf

object AncestryTree {
  def fromElements[E](rootData: E, elements: Seq[E]): Tree[E] = {
      Branch(
      data = rootData, 
      children = elements.map{Leaf[E]})
  }
}
