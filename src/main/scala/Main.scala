import Graph.{Graph, GraphPath}
import SwissborgAPI.Currency
import cats.data.EitherT
import cats.effect.{IO, IOApp}
import cats.implicits._

object Main extends IOApp.Simple {

  //val rawResp = """{"JPY-JPY":"1.0000000","USD-EUR":"0.6905632","BTC-EUR":"96.2921319","USD-BTC":"0.0073698","EUR-BTC":"0.0103040","EUR-USD":"1.1768999","EUR-EUR":"1.0000000","JPY-BTC":"0.0000901","USD-USD":"1.0000000","BTC-BTC":"1.0000000","USD-JPY":"90.9556652","JPY-EUR":"0.0084392","JPY-USD":"0.0108486","BTC-USD":"130.5434075","EUR-JPY":"120.5828525","BTC-JPY":"13363.2344616"}"""
  //val decodedResponse: List[Rate] = decode[List[Row]](rawResp).right.get.map(_.toRate.right.get)

  override def run: IO[Unit] = {
    (for {
      rates <- EitherT(SwissborgAPI.currenciesProgram)
      graph <- EitherT(IO.pure(Right(Graph.fromRates(rates))): IO[Either[String, Graph]])
      allPaths <- EitherT(IO.pure(Right(Graph.allNodesPaths(graph))): IO[Either[String, Map[Currency, List[GraphPath]]]])
      arbitragePaths = getArbitragePaths(allPaths)
    } yield arbitragePaths).value.flatMap {
      case Right(value) => IO.println("Arbitrages found!") *> IO.println(value.mkString(","))
      case Right(value) if value.isEmpty => IO.println("No arbitrages found")
      case Left(error) => IO.raiseError(new Exception(s"Error while calculating Arbitrage $error"))
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
