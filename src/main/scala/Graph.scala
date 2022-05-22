import SwissborgAPI.{Currency, Rate}
import cats.Semigroup

object Graph {

  type GraphNode = (Double, Graph)

  /**
   * This type represents a path from an initial vertex to itself through the edges (cycle)
   */
  type GraphPath = List[GraphNode]

  /**
   * This interface converts from the API response into the Graph
   */
  def fromRates(rates: List[Rate]): Graph = {

    def getNeighbours(currency: Currency, rates: List[Rate]): List[GraphNode] = {
      rates
        .filter(_.currencies._1 == currency)
        .map(rate => (rate.rate, Graph(rate.currencies._2, getNeighbours(rate.currencies._2, rates))))
    }

    val headCurrency = rates.head.currencies._1
    Graph(headCurrency, getNeighbours(headCurrency, rates))
  }

  def apply(currency: Currency, neighbours: => List[GraphNode]) = new Graph(currency, neighbours)

  /**
   * Same as path for for all the Nodes in the graph
   */
  def allNodesPaths(tree: Graph): Map[Currency, List[List[(Double, Graph)]]] = Graph.nodes(tree).view.mapValues(Graph.paths).toMap

  /**
   * Get all the nodes from the graph
   */
  def nodes(tree: Graph): Map[Currency, Graph] = fold(tree)(Map.empty[Currency, Graph])(c => seed => if (seed.contains(c.currency)) seed else seed + (c.currency -> c))(_ => identity)(_ ++ _)

  /**
   * The g function is necessary for getting all the paths, since we need to keep track of the current one and apply a special case when we find a loop in the graph
   * however, for every other operation where the paths are not needed (like getting the nodes), this can be replaced by an identity function
   */
  def fold[A: Semigroup](tree: Graph)(seed: A)(f: Graph => A => A)(g: List[(Double, Graph)] => A => A): A = {

    def foldAux(tail: Graph, alreadyVisited: List[Currency], currentPath: List[(Double, Graph)], seed0: A, isHead: Boolean): A = {
      if (tree.currency == tail.currency && !isHead) g(currentPath)(seed0)
      else f(tail)(tail.neighbours.map {
        case (exchange, childTree) =>
          if (alreadyVisited.contains(childTree.currency)) seed
          else foldAux(childTree, alreadyVisited ++ List(childTree.currency), currentPath ++ List((exchange, childTree)), seed0, isHead = false)
      }.reduce(Semigroup[A].combine))
    }

    foldAux(tree, List.empty, List((1, tree)), seed, isHead = true)
  }

  /**
   * Gets all the possible paths between a node and itself
   */
  def paths(tree: Graph): List[List[(Double, Graph)]] = fold(tree)(List.empty[List[(Double, Graph)]])(_ => identity)(list => seed => seed ++ List(list))

  class Graph(currency0: Currency, neighbours0: => List[GraphNode]) {
    lazy val neighbours: List[GraphNode] = neighbours0
    lazy val currency: Currency = currency0
  }

}
