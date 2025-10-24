package net.collatex.reptilian

import org.scalatest.*
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ArrayBuffer

class traversalGraphPhaseOneTest extends AnyFunSuite:
  test("Identify single shared skipped block") {
    val source = FullDepthBlock(Vector(0, 10), 2) // Skips tokens 2, 3, 4, which we model as one block
    val target = FullDepthBlock(Vector(5, 15), 2)
    val offsets = Map(0 -> ArrayBuffer(0, 0), 2 -> ArrayBuffer(1, 1), 5 -> ArrayBuffer(2, 2))
    val blockOrders = Vector(
      Vector(FullDepthBlock(Vector(0, 10), 2), FullDepthBlock(Vector(2, 12), 3), FullDepthBlock(Vector(5, 15), 2)),
      Vector(FullDepthBlock(Vector(0, 10), 2), FullDepthBlock(Vector(2, 12), 3), FullDepthBlock(Vector(5, 15), 2))
    )
    val expected = Set(FullDepthBlock(Vector(2, 12), 3))
    val result = identifySkippedBlocks(source, target, offsets, blockOrders)
    assert(result == expected)
  }
  test("Identify different skipped blocks") {
    val source = FullDepthBlock(Vector(0, 100), 2) // Skips tokens 2–9, modeled as different blocks for each witness
    val target = FullDepthBlock(Vector(10, 110), 2)
    val offsets = Map(
      0 -> ArrayBuffer(0, 0), // source
      5 -> ArrayBuffer(1, 3), // skipped in second witness, out of range in first
      10 -> ArrayBuffer(2, 2), // target
      50 -> ArrayBuffer(3, 1) // skipped in first witness, out of range in second
    )
    val blockOrders = Vector(
      Vector(
        FullDepthBlock(Vector(0, 100), 2),
        FullDepthBlock(Vector(5, 205), 3), // Skipped in second witness
        FullDepthBlock(Vector(10, 110), 2),
        FullDepthBlock(Vector(50, 105), 3) // Skipped in first witness
      ),
      Vector(
        FullDepthBlock(Vector(0, 100), 2),
        FullDepthBlock(Vector(50, 105), 3), // SKipped in first witness
        FullDepthBlock(Vector(10, 110), 2),
        FullDepthBlock(Vector(5, 205), 3) // Skipped in second witness
      )
    )
    val expected = Set(FullDepthBlock(Vector(5, 205), 3), FullDepthBlock(Vector(50, 105), 3))
    val result = identifySkippedBlocks(source, target, offsets, blockOrders)
    assert(result == result)
  }
