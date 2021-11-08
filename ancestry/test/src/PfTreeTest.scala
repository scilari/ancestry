package com.scilari.ancestry.core
import com.scilari.ancestry.AncestryTree

import org.scalatest._
import flatspec._
import matchers._

import com.scilari.ancestry.helpers.Debug.debugString
import scala.collection.mutable.ArrayBuffer

class PfTreeTests extends AnyFlatSpec with should.Matchers {

  class Particle(val s: String) {
    var weight: Double = 1.0
    val children = ArrayBuffer[Particle]()
    override def toString = s
  }

  object Particle {
    def merge(a: Particle, b: Particle) = Particle(a.s + b.s)
    def breed(p: Particle) = {
      val children = p.children
      p.children.clear()
      children.toList
    }
    def randomChild = Particle(
      if (scala.util.Random.nextDouble() < 0.5) "a" else "b"
    )
  }

  val particleCount = 10

  var tree = AncestryTree.fromElements(Particle("R"), List.fill(particleCount)(Particle("a")))

  def particles = tree.leaves.map { _.data }.toArray
  def weights = particles.map { _.weight }.toArray

  def resampleParticles(particles: Array[Particle], weights: Array[Double]) =
    Resampling.resampleIndices(weights).map { i =>
      particles(i).children += Particle.randomChild
    }

  def updateWeights(tree: Tree[Particle]) = {
    val leaves = tree.leaves
    var sumW = 0.0
    leaves.foreach { leaf =>
      val historySize = 5
      val history = leaf.ancestors.map { _.data.s }.reduce(_ + _).take(historySize)
      val aCount = history.count(_ == 'a')
      val w = aCount / historySize.toDouble
      leaf.data.weight = math.max(w, 0.0001)
      sumW += leaf.data.weight
    }
    val invSum = 1.0 / sumW
    leaves.foreach { _.data.weight *= invSum }
  }

  "Ancestry tree" should "hold its properties when tracking the particles" in {
    for t <- 0 until 10 do
      updateWeights(tree)
      resampleParticles(particles, weights)
      tree = tree.nextGeneration(Particle.breed(_), Particle.merge(_, _)) match {
        case Some(tree) => tree
        case None =>
          println("Something went horribly wrong")
          null
      }
      println(debugString(tree))

      assert(tree.size <= 2 * particleCount + 1, "Tree node size is bounded by O(particleCount)")
      assert(tree.leaves.size == particleCount, "Particle count should stay constant")
      assert(tree.depth <= t + 2, "Depth should not exceed updates")

  }

}
