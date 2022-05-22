import Graph.{GraphPath, getArbitragePaths, showPath, sumPathExchanges}
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
      case Right(arbitragePaths) if arbitragePaths.isEmpty => Console[F].println("No arbitrages found")
      case Right(arbitragePaths) => Console[F].println("Arbitrages found!") *> Console[F].println(showArbitragePath(arbitragePaths).mkString(","))
      case Left(error) => Console[F].println(s"Error while calculating Arbitrage $error")
    }
  }

  val showArbitragePath: List[(Double,GraphPath)] => List[String] = _.map { result =>
      val (total, path: GraphPath) = result
      s"\nPath: ${showPath(path)} - Total value after exchange $total"
    }

}
