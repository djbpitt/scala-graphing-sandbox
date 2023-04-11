package reptilian


import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.*
import Assertions.*
import Inspectors.*
import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef.EdgeAssoc
import scalax.collection.edge.Implicits.edge2WDiEdgeAssoc
import scalax.collection.edge.WDiEdge
import scala.util.matching.Regex

/** Set up fixtures
 *
 * blocks copied from first paragraph of six Darwin witnesses
 */
val blocks: Vector[FullDepthBlock] = Vector(
  FullDepthBlock(Vector(104, 363, 621, 883, 1142, 1394),29),
    FullDepthBlock(Vector(243, 501, 759, 1022, 1274, 1524),17),
    FullDepthBlock(Vector(212, 470, 728, 991, 1245, 1495),12),
    FullDepthBlock(Vector(134, 393, 651, 913, 1172, 1424),7),
    FullDepthBlock(Vector(142, 400, 658, 920, 1179, 1432),2),
    FullDepthBlock(Vector(38, 298, 556, 818, 1082, 1331),1),
    FullDepthBlock(Vector(39, 299, 557, 819, 1079, 1332),3),
    FullDepthBlock(Vector(31, 292, 550, 812, 1073, 1325),6),
    FullDepthBlock(Vector(188, 446, 704, 966, 1221, 1471),3),
    FullDepthBlock(Vector(43, 302, 560, 822, 1083, 1335),16),
    FullDepthBlock(Vector(101, 360, 618, 880, 1140, 1392),2),
    FullDepthBlock(Vector(60, 319, 577, 839, 1101, 1353),31),
    FullDepthBlock(Vector(192, 450, 708, 970, 1225, 1475),6),
    FullDepthBlock(Vector(184, 442, 700, 962, 1219, 1469),2),
    FullDepthBlock(Vector(146, 404, 662, 924, 1183, 1433),25),
    FullDepthBlock(Vector(4, 265, 523, 785, 1047, 1299),26),
    FullDepthBlock(Vector(172, 430, 688, 950, 1208, 1458),11),
    FullDepthBlock(Vector(198, 456, 714, 977, 1232, 1482),12),
    FullDepthBlock(Vector(229, 487, 745, 1008, 1261, 1511),13),
    FullDepthBlock(Vector(225, 483, 741, 1004, 1258, 1508),2),
    FullDepthBlock(Vector(93, 352, 610, 872, 1132, 1384),7)
)

class MajorityOrderTest extends AnyFunSuite:
  test("create graph from blocks") {
    val result = compute_nodes_for_graph(blocks)
    assert(result ==
      Graph(-1, -2, 192, 225, 4, 101, 229, 38, 134, 198, 39, 104, 43, 172, 142, 146, 243, 212, 184, 60, 188, 93, 31))
  }


  test("compute edges for witness") {
    val result = compute_edges_for_witness(blocks, 0)
    assert(result ==
      Vector(DiEdge(4, 31), DiEdge(31, 38), DiEdge(38, 39), DiEdge(39, 43), DiEdge(43, 60), DiEdge(60, 93),
        DiEdge(93, 101), DiEdge(101, 104), DiEdge(104, 134), DiEdge(134, 142), DiEdge(142, 146), DiEdge(146, 172),
        DiEdge(172, 184), DiEdge(184, 188), DiEdge(188, 192), DiEdge(192, 198), DiEdge(198, 212), DiEdge(212, 225),
        DiEdge(225, 229), DiEdge(229, 243), DiEdge(-1, 4), DiEdge(243, -2))
    )
  }

  // TODO: Have we confused from / to?
  test("compute weighted edges") {
    val result = compute_weighted_edges(
      Vector(compute_edges_for_witness(blocks, 0), compute_edges_for_witness(blocks, 1), compute_edges_for_witness(blocks, 4))
    )
    val expected = Vector(
      142 ~> 146 % 3,
      212 ~> 225 % 3,
      172 ~> 184 % 3,
      225 ~> 229 % 3,
      192 ~> 198 % 3,
      -1 ~> 4 % 3,
      31 ~> 39 % 1,
      243 ~> -2 % 3,
      229 ~> 243 % 3,
      43 ~> 60 % 3,
      104 ~> 134 % 3,
      38 ~> 43 % 1,
      146 ~> 172 % 3,
      198 ~> 212 % 3,
      39 ~> 38 % 1,
      60 ~> 93 % 3,
      101 ~> 104 % 3,
      134 ~> 142 % 3,
      31 ~> 38 % 2,
      184 ~> 188 % 3,
      39 ~> 43 % 2,
      4 ~> 31 % 3,
      188 ~> 192 % 3,
      38 ~> 39 % 2,
      93 ~> 101 % 3
    )
    assert(result == expected)
    assert(result.map(_.weight) == expected.map(_.weight))
  }
end MajorityOrderTest
