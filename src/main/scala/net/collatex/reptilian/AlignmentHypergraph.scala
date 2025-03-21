package net.collatex.reptilian

import net.collatex.util.Hypergraph


extension (hg: Hypergraph[EdgeLabel, TokenRange])
  // Cannot call the function 'witnesses' because that clashes with Hyperedge 'witnesses' function causes an infinite loop
  def witnessSet: Set[Int] =
    hg.hyperedges.flatMap(_.witnesses)