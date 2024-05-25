package net.collatex.util

// https://www.geeksforgeeks.org/traveling-salesman-problem-using-branch-and-bound-2/

import java.util._
// Java program to solve Traveling Salesman Problem
// using Branch and Bound.


object tsp {
  val N = 4
  // final_path[] stores the final solution ie, the
  // path of the salesman.
  val final_path = new Array[Int](N + 1)
  // visited[] keeps track of the already visited nodes
  // in a particular path
  val visited = new Array[Boolean](N)
  // Stores the final minimum weight of shortest tour.
  var final_res: Int = Integer.MAX_VALUE

  // Function to copy temporary solution to
  // the final solution
  def copyToFinal(curr_path: Array[Int]): Unit = {
    for (i <- 0 until N) {
      final_path(i) = curr_path(i)
    }
    final_path(N) = curr_path(0)
  }

  // Function to find the minimum edge cost
  // having an end at the vertex i
  def firstMin(adj: Array[Array[Int]], i: Int): Int = {
    var min = Integer.MAX_VALUE
    for (k <- 0 until N) {
      if (adj(i)(k) < min && i != k) min = adj(i)(k)
    }
    min
  }

  // function to find the second minimum edge cost
  // having an end at the vertex i
  def secondMin(adj: Array[Array[Int]], i: Int): Int = {
    var first = Integer.MAX_VALUE
    var second = Integer.MAX_VALUE
    for (j <- 0 until N) {
      if (i != j)
        if (adj(i)(j) <= first) {
          second = first
          first = adj(i)(j)
        }
        else if (adj(i)(j) <= second && adj(i)(j) != first) second = adj(i)(j)
    }
    second
  }

  // function that takes as arguments:
  // curr_bound -> lower bound of the root node
  // curr_weight-> stores the weight of the path so far
  // level-> current level while moving in the search
  //		 space tree
  // curr_path[] -> where the solution is being stored which
  //			 would later be copied to final_path[]
  def TSPRec(adj: Array[Array[Int]], curr_bound: Int, curr_weight: Int, level: Int, curr_path: Array[Int]): Unit = {
    // base case is when we have reached level N which
    // means we have covered all the nodes once
    var curr_weight_copy = curr_weight
    var curr_bound_copy = curr_bound
    if (level == N) {
      // check if there is an edge from last vertex in
      // path back to the first vertex
      if (adj(curr_path(level - 1))(curr_path(0)) != 0) {
        // curr_res has the total weight of the
        // solution we got
        val curr_res = curr_weight + adj(curr_path(level - 1))(curr_path(0))
        // Update final result and final path if
        // current result is better.
        if (curr_res < final_res) {
          copyToFinal(curr_path)
          final_res = curr_res
        }
      }
      return
    }
    // for any other level iterate for all vertices to
    // build the search space tree recursively
    for (i <- 0 until N) {
      // Consider next vertex if it is not same (diagonal
      // entry in adjacency matrix and not visited
      // already)
      if (adj(curr_path(level - 1))(i) != 0 && !visited(i)) {
        val temp = curr_bound_copy
        curr_weight_copy += adj(curr_path(level - 1))(i)
        // different computation of curr_bound for
        // level 2 from the other levels
        if (level == 1) curr_bound_copy -= ((firstMin(adj, curr_path(level - 1)) + firstMin(adj, i)) / 2)
        else curr_bound_copy -= ((secondMin(adj, curr_path(level - 1)) + firstMin(adj, i)) / 2)
        // curr_bound + curr_weight is the actual lower bound
        // for the node that we have arrived on
        // If current lower bound < final_res, we need to explore
        // the node further
        if (curr_bound_copy + curr_weight_copy < final_res) {
          curr_path(level) = i
          visited(i) = true
          // call TSPRec for the next level
          TSPRec(adj, curr_bound_copy, curr_weight_copy, level + 1, curr_path)
        }
        // Else we have to prune the node by resetting
        // all changes to curr_weight and curr_bound
        curr_weight_copy -= adj(curr_path(level - 1))(i)
        curr_bound_copy = temp
        // Also reset the visited array
        java.util.Arrays.fill(visited, false)
        for (j <- 0 until level) {
          visited(curr_path(j)) = true
        }
      }
    }
  }

  // This function sets up final_path[]
  def TSP(adj: Array[Array[Int]]): Unit = {
    val curr_path = new Array[Int](N + 1)
    // Calculate initial lower bound for the root node
    // using the formula 1/2 * (sum of first min +
    // second min) for all edges.
    // Also initialize the curr_path and visited array
    var curr_bound = 0
    java.util.Arrays.fill(curr_path, -1)
    java.util.Arrays.fill(visited, false)
    // Compute initial bound
    for (i <- 0 until N) {
      curr_bound += (firstMin(adj, i) + secondMin(adj, i))
    }
    // Rounding off the lower bound to an integer
    curr_bound = if (curr_bound == 1) curr_bound / 2 + 1
    else curr_bound / 2
    // We start at vertex 1 so the first vertex
    // in curr_path[] is 0
    visited(0) = true
    curr_path(0) = 0
    // Call to TSPRec for curr_weight equal to
    // 0 and level 1
    TSPRec(adj, curr_bound, 0, 1, curr_path)
  }

  // Driver code
  def main(args: Array[String]): Unit = {
    //Adjacency matrix for the given graph
    val adj = Array(Array(0, 10, 15, 20), Array(10, 0, 35, 25), Array(15, 35, 0, 30), Array(20, 25, 30, 0))
    TSP(adj)
    System.out.printf("Minimum cost : %d\n", final_res)
    System.out.printf("Path Taken : ")
    for (i <- 0 to N) {
      System.out.printf("%d ", final_path(i))
    }
  }
}

/* This code contributed by PrinciRaj1992 */
