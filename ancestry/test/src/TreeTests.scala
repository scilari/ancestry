package com.scilari.ancestry.core

import org.scalatest._
import flatspec._
import matchers._

import com.scilari.ancestry.helpers.Debug.debugString

class TreeTests extends AnyFlatSpec with should.Matchers {
  val tree = TreeSource.tree

  "AncestryTree" should "be debug printable" in {
    println("Original tree: " + debugString(tree))
    assert(debugString(tree).size > 10)
  }

  it should "minimize tree and combine elements" in {
    val minimized = tree.nextGeneration(
      breed = (s: String) => Seq(""),
      merge = (s1: String, s2: String) => s1 + s2
    )
    println("Trimmed tree: " + debugString(minimized.get))

  }

  def stringBreed(s: String): Seq[String] = s match {
    case "x"       => Seq("x1", "x2", "x3")
    case "q"       => Seq("q1")
    case "p" | "w" => Seq()
    case _         => Seq("")
  }

  it should "produce offspring" in {
    val next = tree.nextGeneration(
      breed = stringBreed _,
      merge = (s1: String, s2: String) => s1 + s2
    )
    println("Next generation tree: " + debugString(next.get))
  }

  it should "produce correct ancestors" in {
    val xAncestors = tree.find(_.data == "x").get.ancestors.map(_.data)
    println(xAncestors.mkString(" -> "))
    xAncestors should contain theSameElementsInOrderAs List("x", "A", "R")
    val next = tree
      .nextGeneration(
        breed = stringBreed _,
        merge = (s1: String, s2: String) => s1 + s2
      )
      .get
    val x1Ancestors = next.find(_.data == "x1").get.ancestors.map(_.data)
    println(x1Ancestors.mkString(" -> "))
    x1Ancestors should contain theSameElementsInOrderAs List("x1", "x", "A", "R")
  }

  it should "evolve randomly" in {
    var currentTree: Option[Tree[String]] = Some(tree)

    (0 until 10).foreach { i =>
      println(s"Tree $i ${debugString(currentTree.get)}")
      val actualTree = currentTree.get
      val bigEnough = actualTree.nodeCount > 10
      currentTree = actualTree.nextGeneration(
        breed = (_: String) => {
          val r = scala.util.Random.nextDouble()
          if (r < 0.33 && bigEnough) Seq()
          else if (r < 0.67) Seq("g" + i)
          else Seq("g" + i, "h" + i)
        },
        merge = (s1: String, s2: String) => s1 + s2
      )
    }

  }

}
