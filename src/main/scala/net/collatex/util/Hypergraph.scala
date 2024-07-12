package net.collatex.util


// Hypergraph
// @author: Ronald
// Inspired by bipartite adjacency map from alga-graphs

// Hypergraph has hyperedges of type HE and vertices of type V
case class Hypergraph[HE, V](am1: Map[HE, Set[V]], am2: Map[V, Set[HE]])


