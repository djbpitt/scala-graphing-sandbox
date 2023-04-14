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
  FullDepthBlock(Vector(104, 363, 621, 883, 1142, 1394), 29),
  FullDepthBlock(Vector(243, 501, 759, 1022, 1274, 1524), 17),
  FullDepthBlock(Vector(212, 470, 728, 991, 1245, 1495), 12),
  FullDepthBlock(Vector(134, 393, 651, 913, 1172, 1424), 7),
  FullDepthBlock(Vector(142, 400, 658, 920, 1179, 1432), 2),
  FullDepthBlock(Vector(38, 298, 556, 818, 1082, 1331), 1),
  FullDepthBlock(Vector(39, 299, 557, 819, 1079, 1332), 3),
  FullDepthBlock(Vector(31, 292, 550, 812, 1073, 1325), 6),
  FullDepthBlock(Vector(188, 446, 704, 966, 1221, 1471), 3),
  FullDepthBlock(Vector(43, 302, 560, 822, 1083, 1335), 16),
  FullDepthBlock(Vector(101, 360, 618, 880, 1140, 1392), 2),
  FullDepthBlock(Vector(60, 319, 577, 839, 1101, 1353), 31),
  FullDepthBlock(Vector(192, 450, 708, 970, 1225, 1475), 6),
  FullDepthBlock(Vector(184, 442, 700, 962, 1219, 1469), 2),
  FullDepthBlock(Vector(146, 404, 662, 924, 1183, 1433), 25),
  FullDepthBlock(Vector(4, 265, 523, 785, 1047, 1299), 26),
  FullDepthBlock(Vector(172, 430, 688, 950, 1208, 1458), 11),
  FullDepthBlock(Vector(198, 456, 714, 977, 1232, 1482), 12),
  FullDepthBlock(Vector(229, 487, 745, 1008, 1261, 1511), 13),
  FullDepthBlock(Vector(225, 483, 741, 1004, 1258, 1508), 2),
  FullDepthBlock(Vector(93, 352, 610, 872, 1132, 1384), 7)
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
      Vector(
        4 ~> 31,
        31 ~> 38,
        39 ~> 43,
        43 ~> 60,
        60 ~> 93,
        93 ~> 101,
        101 ~> 104,
        104 ~> 134,
        134 ~> 142,
        142 ~> 146,
        146 ~> 172,
        172 ~> 184,
        184 ~> 188,
        188 ~> 192,
        192 ~> 198,
        198 ~> 212,
        212 ~> 225,
        225 ~> 229,
        229 ~> 243,
        -1 ~> 4,
        243 ~> -2
      )
    )
  }

  test("compute weighted edges") {
    val result = compute_weighted_edges(
      Vector(
        compute_edges_for_witness(blocks, 0),
        compute_edges_for_witness(blocks, 1),
        compute_edges_for_witness(blocks, 4)
      )
    )
    val expected = Vector(
      -1 ~> 4 % 26,
      4 ~> 31 % 6,
      31 ~> 38 % 1,
      31 ~> 39 % 3,
      38 ~> 43 % 16,
      39 ~> 43 % 16,
      43 ~> 60 % 31,
      60 ~> 93 % 7,
      93 ~> 101 % 2,
      101 ~> 104 % 29,
      104 ~> 134 % 7,
      134 ~> 142 % 2,
      142 ~> 146 % 25,
      146 ~> 172 % 11,
      172 ~> 184 % 2,
      184 ~> 188 % 3,
      188 ~> 192 % 6,
      192 ~> 198 % 12,
      198 ~> 212 % 12,
      212 ~> 225 % 2,
      225 ~> 229 % 13,
      229 ~> 243 % 17,
      243 ~> -2 % 0
    )
    assert(result.sortBy(e => e.from) == expected)
    assert(result.sortBy(e => e.from).map(_.weight) == expected.map(_.weight))
  }

  test("create traversal graph") {
    val result = create_traversal_graph(blocks)
    // note: the weights om the edges are not checked in this assert!
    assert(result ==
      Graph(-2, -1, 4, 31, 38, 39, 43, 60, 93, 101, 104, 134, 142,
        146, 172, 184, 188, 192, 198, 212, 225, 229, 243,
        -1 ~> 4 % 6.0, 4 ~> 31 % 6.0, 31 ~> 38 % 5.0, 31 ~> 39 % 1.0,
        38 ~> 43 % 1.0, 39 ~> 43 % 5.0, 43 ~> 60 % 6.0, 60 ~> 93 % 6.0,
        93 ~> 101 % 6.0, 101 ~> 104 % 6.0, 104 ~> 134 % 6.0, 134 ~> 142 % 6.0,
        142 ~> 146 % 6.0, 146 ~> 172 % 6.0, 172 ~> 184 % 6.0, 184 ~> 188 % 6.0,
        188 ~> 192 % 6.0, 192 ~> 198 % 6.0, 198 ~> 212 % 6.0, 212 ~> 225 % 6.0,
        225 ~> 229 % 6.0, 229 ~> 243 % 6.0, 243 ~> -2 % 6.0)
    )
  }

  test("create dot file") {
    val g = create_traversal_graph(blocks)
    val result = graph_to_dot(g)
    assert(result == result)
    val outputPath = os.pwd / "src" / "main" / "output" / "alignment.dot"
    os.write.over(outputPath, result)
  }
end MajorityOrderTest
