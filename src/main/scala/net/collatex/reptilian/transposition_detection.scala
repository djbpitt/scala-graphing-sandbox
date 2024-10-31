/*
 * When aligning two hyper-graphs we have to detect transpositions between the two graphs.
 * To detect transpositions we need have order, as we do for creating a variant graph or an 
 * alignment table.
 *
 * We run into the problem that the Token ranges, and thus the Hyperedges in an alignment 
 * hypergraph, are partially ordered. Token ranges only state something about a single witness, 
 * which means that two token ranges within the same witness can be compared and their relative
 * order determined. Two token ranges of different witnesses however can't be compared.
 *
 * Partially ordered items cannot be sorted the traditional way because not all the items can be
 * compared. To sort them we have to create a dependency graph and then topologically sort the 
 * nodes in the graph.
 *
 * 1. Create a dependency graph for each of the hyper-graphs:
 *    a. Create 'fake' hyperedges for the start and end nodes and find the start and end 
 *       coordinates in the global token array
 *    b. Go over the hyperedges in each hypergraph to ap the hyper-graphs to a sorted 
 *       Map[Int, String], where Int is the position where a token range starts in the global 
 *       token array and String is the edge label where the data comes from
 *    d. Create the outgoing edges for the dependency graph by going over the hyperedges and 
 *       using the sorted to determine what the next item in the map is for the positions in 
 *       each of the witnesses.
 *    This finds the hyperedges, which are the nodes in the dependency graph to connect to. 
 *    De-duplicate the edges because when witnesses will point to the same ones.
 * 2. Topological sort the dependency graph
 * 3. Rank the hyperedges
 * 4. Create a traversal/decision graph for the traversal of the two sorted and ranked hyperedges
 * 5. Beam search the traversal graph
 * */