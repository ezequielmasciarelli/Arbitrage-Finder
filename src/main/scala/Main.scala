import cats.{Applicative, Semigroup}
import cats.data.EitherT
import cats.effect.{IO, IOApp}
import cats.implicits._
import io.circe.Decoder
import org.http4s.circe.jsonOf
import org.http4s.ember.client.EmberClientBuilder
import io.circe.parser.decode


object Main extends IOApp.Simple {

  case class Currency(value: String)

  case class Row(currencies: String, rate: Double) {
    def toRate: Either[String, Rate] = {
      currencies.split("-").map(Currency.apply).toList match {
        case c1 :: c2 :: Nil => Right(Rate((c1, c2), rate))
        case _ => Left("Error while converting response to Rate")
      }
    }
  }

  case class Rate(currencies: (Currency, Currency), rate: Double)

  type Result[T] = IO[Either[String, T]]


  implicit val responseDecoder: Decoder[List[Row]] = for {
    map <- Decoder.decodeMap[String, Double]
    rows = map.map(Row.tupled)
  } yield rows.toList

  val currenciesProgram: Result[List[Rate]] = EmberClientBuilder.default[IO].build.use { client =>

    for {
      response <- client.expect[List[Row]]("https://api.swissborg.io/v1/challenge/rates")(jsonOf[IO, List[Row]])
      currencies = response.map(_.toRate).sequence
    } yield currencies
  }

  class LazyTree(currency0: Currency, neighbours0: => List[(Double, LazyTree)]) {
    lazy val neighbours: List[(Double, LazyTree)] = neighbours0
    lazy val currency: Currency = currency0
  }

  object LazyTree {
    def apply(currency: Currency, neighbours: => List[(Double, LazyTree)]) = new LazyTree(currency, neighbours)

    /**
     * The g function is necesary for getting all the paths, since we need to keep track of the current and apply a special case when we find a loop in the graph
     */
    def fold[A: Semigroup](tree: LazyTree)(seed: A)(f: LazyTree => A => A)(g: List[(Double,LazyTree)] => A => A): A = {

      def foldAux(tail: LazyTree, alreadyVisited: List[Currency], currentPath: List[(Double,LazyTree)], seed0: A, isHead: Boolean): A = {
        if (tree.currency == tail.currency && !isHead) g(currentPath)(seed0)
        else f(tail)(tail.neighbours.map {
          case (exchange, childTree) =>
            if (alreadyVisited.contains(childTree.currency)) seed
            else foldAux(childTree, alreadyVisited ++ List(childTree.currency), currentPath ++ List((exchange,childTree)), seed0, isHead = false)
        }.reduce(Semigroup[A].combine))
      }
      foldAux(tree, List.empty, List((1,tree)), seed, isHead = true)
    }

    def currencies(tree: LazyTree): Set[Currency] = fold(tree)(Set.empty[Currency])(c => seed => seed + c.currency)(_ => identity)

    /** WHY MAPs ARE NOT SEMIGROUPS? */
    def nodes(tree: LazyTree): Map[Currency,LazyTree] = fold(tree)(Map.empty[Currency,LazyTree])(c => seed => if(seed.contains(c.currency)) seed else seed + (c.currency -> c))(_ => identity)(_ ++ _)

    /**
     * Gets all the possible paths between a node and itself
     */
    def paths(tree: LazyTree): List[List[(Double,LazyTree)]] = fold(tree)(List.empty[List[(Double,LazyTree)]])(_ => identity)(list => seed => seed ++ List(list))

    /**
     * Same as path for for all the Nodes in the graph
     */
    def allNodesPaths(tree:LazyTree): Map[Currency,List[List[(Double,LazyTree)]]] = LazyTree.nodes(tree).view.mapValues(LazyTree.paths).toMap

  }

  def toTree(l: List[Rate]): LazyTree = {

    def getNeighbours(currency: Currency, rates: List[Rate]): List[(Double, LazyTree)] = {
      rates
        .filter(_.currencies._1 == currency)
        .map(rate => (rate.rate, LazyTree(rate.currencies._2, getNeighbours(rate.currencies._2, rates))))
    }

    val headCurrency = l.head.currencies._1
    LazyTree(headCurrency, getNeighbours(headCurrency, l))
  }


  def isArbitrage: List[(Double, LazyTree)] => Boolean = getLoopAmount(_) > 1
  def getLoopAmount: List[(Double, LazyTree)] => Double = _.map(_._1).fold(1:Double)(_ * _)

  def getArbitragePaths(allPaths: Map[Currency, List[List[(Double, LazyTree)]]]): List[String] = {
    allPaths.values.flatten.toList.map(path => (getLoopAmount(path),path)).filter(_._1 > 1).sortBy(_._1).map { a =>
      val (total,path) = a
      s"\nPath: ${path.map(_._2).map(_.currency.value).mkString("-")} - Total value after exchange $total"
    }
  }

  override def run: IO[Unit] = {
    (for {
      rates <- EitherT(currenciesProgram)
      graph <- EitherT(IO.pure(Right(toTree(rates))): IO[Either[String, LazyTree]])
      allPaths <- EitherT(IO.pure(Right(LazyTree.allNodesPaths(graph))): IO[Either[String, Map[Currency,List[List[(Double,LazyTree)]]]]])
      arbitragePaths = getArbitragePaths(allPaths)
    } yield arbitragePaths).value.flatMap {
      case Right(value) => IO.println("Arbitrages found!") *> IO.println(value.mkString(","))
      case Right(value) if value.isEmpty => IO.println("No arbitrages found")
      case Left(error) => IO.raiseError(new Exception(s"Error while calculating Arbitrage $error"))
    }
  }
}
