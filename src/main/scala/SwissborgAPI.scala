import cats.effect.Async
import cats.implicits._
import io.circe.Decoder
import org.http4s.circe.jsonOf
import org.http4s.ember.client.EmberClientBuilder

object SwissborgAPI {
  type Result[F[_],T] = F[Either[String, T]]

  implicit val responseDecoder: Decoder[List[Row]] = for {
    map <- Decoder.decodeMap[String, Double]
    rows = map.map(Row.tupled)
  } yield rows.toList

  def currenciesProgram[F[_]: Async]: Result[F,List[Rate]] = EmberClientBuilder.default[F].build.use { client =>

    for {
      response <- client.expect[List[Row]]("https://api.swissborg.io/v1/challenge/rates")(jsonOf[F, List[Row]])
      currencies = response.map(_.toRate).sequence
    } yield currencies
  }

  case class Currency(value: String)

  case class Rate(currencies: (Currency, Currency), rate: Double)

  case class Row(currencies: String, rate: Double) {
    def toRate: Either[String, Rate] = {
      currencies.split("-").map(Currency.apply).toList match {
        case c1 :: c2 :: Nil => Right(Rate((c1, c2), rate))
        case _ => Left("Error while converting response to Rate")
      }
    }
  }
}
