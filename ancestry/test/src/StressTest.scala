package com.scilari.ancestry.core
import com.scilari.ancestry.AncestryTree

import org.scalatest._
import flatspec._
import matchers._

import com.scilari.ancestry.helpers.Debug.debugString
import scala.collection.mutable.ArrayBuffer

class StressTest extends AnyFlatSpec with should.Matchers {

  class Particle(val k: Int) {
    var weight: Double = 1.0
    val children = ArrayBuffer[Particle]()
    override def toString = k.toString
  }

  object Particle {
    def merge(a: Particle, b: Particle) = Particle(a.k + b.k)
    def breed(p: Particle) = {
      val children = p.children.toSeq
      p.children.clear()
      children
    }
    def randomChild = Particle(
      if (scala.util.Random.nextDouble() < 0.5) 1 else 2
    )
  }

  // Selected for stress test to match the amount of particles used in FootSLAM
  val particleCount = 30000

  var tree = AncestryTree.fromElements(Particle(1), Seq.fill(particleCount)(Particle(1)))

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
      leaf.data.weight = leaf.data.k.toDouble
      sumW += leaf.data.weight
    }
    val invSum = 1.0 / sumW
    leaves.foreach { _.data.weight *= invSum }
  }

  "Ancestry tree" should s"handle ${particleCount} particles" in {
    val t0 = System.currentTimeMillis()
    val updateCount = 500
    for t <- 0 until updateCount do
      if (t % 100 == 0) println(s"Stress test iteration $t/$updateCount")
      updateWeights(tree)
      resampleParticles(particles, weights)
      tree = tree.nextGeneration(Particle.breed(_), Particle.merge(_, _)) match {
        case Some(tree) => tree
        case None =>
          println("Something went horribly wrong")
          null
      }

    //assert(tree.size <= 2 * particleCount + 1, "Tree node size is bounded by O(particleCount)")
    //assert(tree.leafCount == particleCount, "Particle count should stay constant")
    //assert(tree.depth <= t + 2, "Depth should not exceed updates")

    val dt = System.currentTimeMillis - t0
    info(
      s"Updating $updateCount times took $dt ms. Time per update ${dt / updateCount.toDouble} ms."
    )
    // println(debugString(tree))

  }

}
