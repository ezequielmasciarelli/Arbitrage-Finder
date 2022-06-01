import SwissborgAPI.{Currency, Rate}
import cats.{Monoid, Semigroup}

object Graph {

  type GraphNode = (Double,Graph)

  /**
   * This type represents a path from an initial vertex to itself through the edges (cycle)
   */
  type GraphPath = List[GraphNode]

  /**
   * This interface converts from the API response into the Graph
   */
  def apply(rates: List[Rate]): Graph = {

    def getNeighbours(currency: Currency, rates: List[Rate]): GraphPath = {
      rates
        .filter(_.currencies._1 == currency)
        .map(rate => (rate.rate, Graph(rate.currencies._2, getNeighbours(rate.currencies._2, rates))))
    }

    val headCurrency = rates.head.currencies._1
    Graph(headCurrency, getNeighbours(headCurrency, rates))
  }

  def apply(currency: Currency, neighbours: => GraphPath) = new Graph(currency, neighbours)

  /**
   * Get all the nodes from the graph
   */
  def vertexes(graph: Graph): Map[Currency, Graph] = reduce[Map[Currency, Graph]](graph)(c => seed => if (seed.contains(c.currency)) seed else seed + (c.currency -> c))(_ => identity)(new Monoid[Map[Currency,Graph]] {
    override def empty: Map[Currency, Graph] = Map.empty
    override def combine(x: Map[Currency, Graph], y: Map[Currency, Graph]): Map[Currency, Graph] = x ++ y
  })

  /**
   * Gets all the possible paths between a node and itself
   */
  def paths(graph: Graph): List[GraphPath] = reduce[List[GraphPath]](graph)(_ => identity)(list => seed => seed ++ List(list))

  /**
   * Same as path for for all the Nodes in the graph
   */
  def allNodesPaths(graph: Graph): Map[Currency, List[GraphPath]] = Graph.vertexes(graph).view.mapValues(Graph.paths).toMap

  /**
   * Get all the currencies, used only on the tests
   */
  def currencies(graph: Graph): Set[Currency] = reduce[Set[Currency]](graph)(c => seed => seed + c.currency)(_ => identity)

  /**
   * The g function is necessary for getting all the paths, since we need to keep track of the current one and apply a special case when we find a loop in the graph
   * however, for every other operation where the paths are not needed (like getting the nodes), this can be replaced by an identity function
   */
  def reduce[A: Monoid](graph: Graph)(f: Graph => A => A)(g: GraphPath => A => A): A = {

    def foldAux(tail: Graph, alreadyVisited: List[Currency], currentPath: GraphPath, isHead: Boolean): A = {
      if (graph.currency == tail.currency && !isHead) g(currentPath)(Monoid[A].empty)
      else f(tail)(tail.neighbours.map {
        case (exchange, childGraph) =>
          if (alreadyVisited.contains(childGraph.currency)) Monoid[A].empty
          else foldAux(childGraph, alreadyVisited ++ List(childGraph.currency), currentPath ++ List((exchange, childGraph)), isHead = false)
      }.reduce(Semigroup[A].combine))
    }

    foldAux(graph, List.empty, List((1, graph)), isHead = true)
  }

  /** *
   * Sums all the paths exchanges. A number greater than 1 means that we found an Arbitrage
   */
  def sumPathExchanges: GraphPath => Double = _.map(_._1).fold(1: Double)(_ * _)

  def showPath(path: GraphPath): String = path.map(_._2).map(_.currency.value).mkString("-")

  class Graph(currency0: Currency, neighbours0: => GraphPath) {
    lazy val neighbours: GraphPath = neighbours0
    lazy val currency: Currency = currency0
  }

  /**
   * Gets all the paths where you end up with more money than when you began
   */
  def getArbitragePaths(allPaths: Map[Currency, List[GraphPath]]): List[(Double, GraphPath)] = {
    allPaths.values.flatten.toList.map(path => (sumPathExchanges(path), path)).filter(_._1 > 1).sortBy(_._1)
  }

}
