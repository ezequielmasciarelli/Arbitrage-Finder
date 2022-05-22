import cats.effect.{IO, IOApp}
import io.circe.Decoder
import org.http4s.circe.jsonOf
import org.http4s.ember.client.EmberClientBuilder
import cats.implicits._
import cats.data.EitherT
import io.circe.parser.decode


import scala.annotation.tailrec

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

    def fold[A, B](tree: LazyTree)(seed: A)(f: Currency => List[A] => A)(g: A => List[Currency] => A): A = {

      def foldAux(tail: LazyTree, alreadyVisited: List[Currency], currentList: List[Currency], seed0: A, isHead: Boolean): A = {
        if (tree.currency == tail.currency && !isHead) g(seed0)(currentList)
        else f(tail.currency)(tail.neighbours.map {
          case (_, childTree) =>
            if (alreadyVisited.contains(childTree.currency)) seed
            else foldAux(childTree, alreadyVisited ++ List(childTree.currency), currentList ++ List(childTree.currency), seed0, isHead = false)
        })
      }

      val r = foldAux(tree, List.empty, List(tree.currency), seed, isHead = true)
      r
    }

    /**
     * ESTA FUNCION RECORRE TODOS LOS EDGES DEL GRAFO, EN TOTAL 16 EDGES QUE CONECTAN CADA NODO (VERTEX)
     */
    def currencies(tree: LazyTree): Set[Currency] = fold(tree)(Set.empty[Currency])(c => seed => (c::seed.flatten).toSet)(seed => _ => seed)
    //def currencies2(tree: LazyTree): String = fold(tree)("")(c => seed => c.value ++ seed.mkString)(a => "")

    def minoPaths(tree: LazyTree): List[List[Currency]] = fold(tree)(List.empty[List[Currency]])(_ => l => l.flatten)(seed => list => seed ++ List(list))

    def paths[A, B](tree: LazyTree): List[List[Currency]] = {

      def pathAux(tail: LazyTree, alreadyVisited: List[Currency], currentList: List[Currency], totalLists: List[List[Currency]], isHead: Boolean): List[List[Currency]] = {
        if (tree.currency == tail.currency && !isHead) totalLists ++ List(currentList)
        else tail.neighbours.flatMap {
          case (_, childTree) =>
            if (alreadyVisited.contains(childTree.currency)) List.empty
            else pathAux(childTree, alreadyVisited ++ List(childTree.currency), currentList ++ List(childTree.currency), totalLists, isHead = false)
        }
      }

      val r = pathAux(tree, List.empty, List(tree.currency), List.empty, isHead = true)
      r
    }
  }

  //val rawResp = """{"USD-EUR":"0.7473154","BTC-EUR":"100.7655938","USD-BTC":"0.0079755","EUR-BTC":"0.0097373","EUR-USD":"1.1121764","EUR-EUR":"1.0000000","JPY-BTC":"0.0000842","USD-USD":"1.0000000","BTC-BTC":"1.0000000","USD-JPY":"98.4306366","JPY-EUR":"0.0078853","JPY-USD":"0.0101366","BTC-USD":"136.6080875","EUR-JPY":"113.9514154","BTC-JPY":"13984.0527988"}"""
  //val decodedResponse: List[Row] = decode[List[Row]](rawResp).right.get

  // val (a: Foo, b: Foo,c: Foo) = (Foo("a", List(b,c)), Foo("b", List(c)), Foo("c",List(a)))
  //val (a: LazyTree, b: LazyTree) = (LazyTree(Currency("USD"),List(("200",b: LazyTree))) , LazyTree(Currency("ARS"),List(("0.005",a: LazyTree))))
  //println(a)
  val rates: List[Rate] = List(Rate((Currency("ARS"), Currency("USD")), "200"), Rate((Currency("USD"), Currency("ARS")), "0.005"))

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


  override def run: IO[Unit] = (for {
    currencies <- EitherT(currenciesProgram)
    tree <- EitherT(IO.pure(Right(toTree(currencies))): IO[Either[String, LazyTree]])
    path <- EitherT(IO.pure(Right(LazyTree.currencies(tree))): IO[Either[String, Set[Currency]]])
    path2 <- EitherT(IO.pure(Right(LazyTree.minoPaths(tree))): IO[Either[String, List[List[Currency]]]])
    _ <- EitherT(IO.println(tree).map(Right(_)): IO[Either[String, Unit]])
  } yield ()).value.void
}
