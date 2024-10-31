/*
 * When aligning two hyper-graphs we have to detected transpositions between the two graphs.
 * To detect transpositions we need have order. We run into the problem that the Hyperedges in a alignment hypergraph are partially ordered.
 * Partially ordered items cannot be sorted the traditional way, because not all the items can be compared. 
 * To sort them we have to create a dependency graph and then topologically sort the nodes in the graph.
 * 
 * 1. Create a dependency graph for each of the hyper-graphs
 * a. Create 'fake' hyperedges for the start and end nodes, find the start and end coordinates in the global token array
 * b. Go over the hyperedges in each hypergraph
 * c. Map the hyper-graphs to sorted map[Int, String], where int is the position where a token range start in the global token array
 * and String is the edge label where the data comes from
 * 2. Topological sort the dependency graph
 * 3. Rank the hyperedges
 * 4. Create a traversal/decision graph for the traversal of the two sorted and ranked hyperedges
 * 5. Beam search the traversal graph
 * */