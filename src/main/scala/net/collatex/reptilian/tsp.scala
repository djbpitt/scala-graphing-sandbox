package net.collatex.reptilian
//https://www.interviewbit.com/blog/travelling-salesman-problem/
import java.util._
import scala.util.boundary, boundary.break

object Main_tse {

  def travllingSalesmanProblem(graph: Array[Array[Int]], s: Int, v: Int): Int = {
    val vertex = java.util.ArrayList[Integer]
    val route = Vector[(Int, Int)]
    for (i <- 0 until v) {
      if (i != s) vertex.add(i)
    }
    println(vertex)
    var min_path = Integer.MAX_VALUE
    while (findNextPermutation(vertex)) {
      var current_pathweight = 0
      var k = s
      for (i <- 0 until vertex.size) {
        current_pathweight += graph(k)(vertex.get(i))
        k = vertex.get(i)
      }
      current_pathweight += graph(k)(s)
      route.addElement((k, s))
      min_path = Math.min(min_path, current_pathweight)
    } 
    println(route)
    min_path
  }

  def swap(data: java.util.ArrayList[Integer], left: Int, right: Int): java.util.ArrayList[Integer] = {
    val temp = data.get(left)
    data.set(left, data.get(right))
    data.set(right, temp)
    data
  }

  def reverse(data: java.util.ArrayList[Integer], left_copy: Int, right_copy: Int): java.util.ArrayList[Integer] = {
    var left = left_copy
    var right = right_copy
    while (left < right) {
      val temp = data.get(left)
      data.set({
        left += 1;
        left - 1
      }, data.get(right))
      data.set({
        right -= 1;
        right + 1
      }, temp)
    }
    data
  }

  def findNextPermutation(data_copy: java.util.ArrayList[Integer]): Boolean = {
    var data = data_copy
    if (data.size <= 1) return false
    var last = data.size - 2
    boundary {
      while (last >= 0) {
        if (data.get(last) < data.get(last + 1)) then break()
        last -= 1
      }
    }
    if (last < 0) return false
    var nextGreater = data.size - 1
    for (i <- data.size - 1 until last by -1) {
      boundary {
        if (data.get(i) > data.get(last)) {
          nextGreater = i
          break() //todo: break is not supported
        }
      }
    }
    data = swap(data, nextGreater, last)
    data = reverse(data, last + 1, data.size - 1)
    println(s"data: $data")
    true
  }

  @main
  def main_tse(): Unit = {
    val graph = Array(Array(0, 10, 15, 20), Array(10, 0, 35, 25), Array(15, 35, 0, 30), Array(20, 25, 30, 0))
    val graph1 = Array(Array(0, 10, 15, 20, 0), Array(10, 0, 35, 25, 0), Array(15, 35, 0, 30, 0), Array(20, 25, 30, 0, 0), Array(0, 0, 0, 0, 0))
    val s = 0
    System.out.println(travllingSalesmanProblem(graph1, s, 5))
  }
}
/*

00 10 15 20
10 00 35 25
15 35 00 30
20 25 30 00


00 10 15 20 00
10 00 35 25 00
15 35 00 30 00
20 25 30 00 00
00 00 00 00 00

* */