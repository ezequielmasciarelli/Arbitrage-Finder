import cats.{Applicative, Semigroup}
import cats.data.EitherT
import cats.effect.{IO, IOApp}
import cats.implicits._
import io.circe.Decoder
import org.http4s.circe.jsonOf
import org.http4s.ember.client.EmberClientBuilder

object Main extends IOApp.Simple {

  case class Currency(value: String)

  case class Row(currencies: String, rate: String) {
    def toRate: Either[String, Rate] = {
      currencies.split("-").map(Currency.apply).toList match {
        case c1 :: c2 :: Nil => Right(Rate((c1, c2), rate))
        case _ => Left("Error while converting response to Rate")
      }
    }
  }

  case class Rate(currencies: (Currency, Currency), rate: String)

  type Result[T] = IO[Either[String, T]]


  implicit val responseDecoder: Decoder[List[Row]] = for {
    map <- Decoder.decodeMap[String, String]
    rows = map.map(Row.tupled)
  } yield rows.toList

  val currenciesProgram: Result[List[Rate]] = EmberClientBuilder.default[IO].build.use { client =>

    for {
      response <- client.expect[List[Row]]("https://api.swissborg.io/v1/challenge/rates")(jsonOf[IO, List[Row]])
      currencies = response.map(_.toRate).sequence
    } yield currencies
  }

  class LazyTree(currency0: Currency, neighbours0: => List[(String, LazyTree)]) {
    lazy val neighbours: List[(String, LazyTree)] = neighbours0
    lazy val currency: Currency = currency0
  }

  object LazyTree {
    def apply(currency: Currency, neighbours: => List[(String, LazyTree)]) = new LazyTree(currency, neighbours)

    /**
     * The g function is necesary for getting all the paths, since we need to keep track of the current and apply a special case when we find a loop in the graph
     */
    def fold[A: Semigroup](tree: LazyTree)(seed: A)(f: LazyTree => A => A)(g: List[(String,LazyTree)] => A => A): A = {

      def foldAux(tail: LazyTree, alreadyVisited: List[Currency], currentPath: List[(String,LazyTree)], seed0: A, isHead: Boolean): A = {
        if (tree.currency == tail.currency && !isHead) g(currentPath)(seed0)
        else f(tail)(tail.neighbours.map {
          case (exchange, childTree) =>
            if (alreadyVisited.contains(childTree.currency)) seed
            else foldAux(childTree, alreadyVisited ++ List(childTree.currency), currentPath ++ List((exchange,childTree)), seed0, isHead = false)
        }.reduce(Semigroup[A].combine))
      }
      foldAux(tree, List.empty, List(("0",tree)), seed, isHead = true)
    }

    def currencies(tree: LazyTree): Set[Currency] = fold(tree)(Set.empty[Currency])(c => seed => seed + c.currency)(_ => identity)

    /** WHY MAPs ARE NOT SEMIGROUPS? */
    def nodes(tree: LazyTree): Map[Currency,LazyTree] = fold(tree)(Map.empty[Currency,LazyTree])(c => seed => if(seed.contains(c.currency)) seed else seed + (c.currency -> c))(_ => identity)(_ ++ _)

    /**
     * Gets all the possible paths between a node and itself
     */
    def paths(tree: LazyTree): List[List[(String,LazyTree)]] = fold(tree)(List.empty[List[(String,LazyTree)]])(_ => identity)(list => seed => seed ++ List(list))

    /**
     * Same as path for for all the Nodes in the graph
     */
    def allNodesPaths(tree:LazyTree): Map[Currency,List[List[(String,LazyTree)]]] = LazyTree.nodes(tree).view.mapValues(LazyTree.paths).toMap

  }

  //val rawResp = """{"USD-EUR":"0.7473154","BTC-EUR":"100.7655938","USD-BTC":"0.0079755","EUR-BTC":"0.0097373","EUR-USD":"1.1121764","EUR-EUR":"1.0000000","JPY-BTC":"0.0000842","USD-USD":"1.0000000","BTC-BTC":"1.0000000","USD-JPY":"98.4306366","JPY-EUR":"0.0078853","JPY-USD":"0.0101366","BTC-USD":"136.6080875","EUR-JPY":"113.9514154","BTC-JPY":"13984.0527988"}"""
  //val decodedResponse: List[Row] = decode[List[Row]](rawResp).right.get

  // val (a: Foo, b: Foo,c: Foo) = (Foo("a", List(b,c)), Foo("b", List(c)), Foo("c",List(a)))
  //val (a: LazyTree, b: LazyTree) = (LazyTree(Currency("USD"),List(("200",b: LazyTree))) , LazyTree(Currency("ARS"),List(("0.005",a: LazyTree))))
  //println(a)
  //val rates: List[Rate] = List(Rate((Currency("ARS"), Currency("USD")), "200"), Rate((Currency("USD"), Currency("ARS")), "0.005"))

  def toTree(l: List[Rate]): LazyTree = {

    def getNeighbours(currency: Currency, rates: List[Rate]): List[(String, LazyTree)] = {
      rates
        .filter(_.currencies._1 == currency)
        .map(rate => (rate.rate, LazyTree(rate.currencies._2, getNeighbours(rate.currencies._2, rates))))
    }

    val headCurrency = l.head.currencies._1
    LazyTree(headCurrency, getNeighbours(headCurrency, l))
  }

  //val explosion = toTree(rates)
  //println(explosion)


  override def run: IO[Unit] = {
    (for {
      rates <- EitherT(currenciesProgram)
      graph <- EitherT(IO.pure(Right(toTree(rates))): IO[Either[String, LazyTree]])
      nodes <- EitherT(IO.pure(Right(LazyTree.nodes(graph))): IO[Either[String, Map[Currency,LazyTree]]])
      allPaths <- EitherT(IO.pure(Right(LazyTree.allNodesPaths(graph))): IO[Either[String, Map[Currency,List[List[(String,LazyTree)]]]]])
      asd = 1
    } yield ()).value.void *> IO.println("DONE")
  }
}
