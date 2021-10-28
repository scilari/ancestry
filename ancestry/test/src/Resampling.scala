package com.scilari.ancestry.core

import scala.collection.mutable.ArrayBuffer

object Resampling {

  def resampleIndices(weights: Array[Double]): Array[Int] = {
    val step = 1.0 / weights.size
    var wSum = scala.util.Random.between(0, step)
    val cs = weights.scanLeft(0.0)(_ + _).tail
    var pIx = 0
    val newIndices = ArrayBuffer[Int]()
    while (wSum < 1.0) {
      if (cs(pIx) >= wSum) {
        newIndices += pIx
        wSum += step
      } else pIx += 1
    }
    newIndices.toArray
  }
}
