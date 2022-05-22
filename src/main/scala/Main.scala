import Graph.GraphPath
import SwissborgAPI.Currency
import cats.Applicative
import cats.data.EitherT
import cats.effect.std.Console
import cats.effect.{Async, IO, IOApp}
import cats.implicits._

object Main extends IOApp.Simple {

  override def run: IO[Unit] = runF[IO]

  def runF[F[_]: Applicative: Async: Console]: F[Unit] = {
    (for {
      rates <- EitherT(SwissborgAPI.currenciesProgram)
      graph <- EitherT.pure[F,String](Graph(rates))
      allPaths <- EitherT.pure[F,String](Graph.allNodesPaths(graph))
      arbitragePaths = getArbitragePaths(allPaths)
    } yield arbitragePaths).value.flatMap {
      case Right(value) if value.isEmpty => Console[F].println("No arbitrages found")
      case Right(value) => Console[F].println("Arbitrages found!") *> Console[F].println(value.mkString(","))
      case Left(error) => Console[F].println(s"Error while calculating Arbitrage $error")
    }
  }

  def getArbitragePaths(allPaths: Map[Currency, List[GraphPath]]): List[String] = {

    /** *
     * Sums all the paths exchanges. A number greater than 1 means that we found an Arbitrage
     */
    def sumPathExchanges: GraphPath => Double = _.map(_._1).fold(1: Double)(_ * _)

    def showPath(path: GraphPath): String = path.map(_._2).map(_.currency.value).mkString("-")

    allPaths.values.flatten.toList.map(path => (sumPathExchanges(path), path)).filter(_._1 > 1).sortBy(_._1).map { result =>
      val (total, path: GraphPath) = result
      s"\nPath: ${showPath(path)} - Total value after exchange $total"
    }
  }

}
