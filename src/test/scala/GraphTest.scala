import Graph.showPath
import SwissborgAPI.{Currency, Rate, Row}
import io.circe.parser._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._


class GraphTest extends AnyFlatSpec {

  val allCurrencies = List(Currency("EUR"), Currency("USD"), Currency("BTC"), Currency("JPY"))

  private def getMockResponse: List[Rate] = {
    val rawResp = """{"JPY-JPY":"1.0000000","USD-EUR":"0.6905632","BTC-EUR":"96.2921319","USD-BTC":"0.0073698","EUR-BTC":"0.0103040","EUR-USD":"1.1768999","EUR-EUR":"1.0000000","JPY-BTC":"0.0000901","USD-USD":"1.0000000","BTC-BTC":"1.0000000","USD-JPY":"90.9556652","JPY-EUR":"0.0084392","JPY-USD":"0.0108486","BTC-USD":"130.5434075","EUR-JPY":"120.5828525","BTC-JPY":"13363.2344616"}"""
    decode[List[Row]](rawResp).right.get.map(_.toRate.right.get) // UNSAFE, but is only for this quick test
  }

  "The API decoder" should "decode the response" in {
    val decodedResponse = getMockResponse
    decodedResponse.length shouldBe 16
    decodedResponse.map(_.currencies).flatMap { case (currency, currency1) => List(currency, currency1) } should contain allElementsOf allCurrencies
  }

  it should "return all the currencies" in {
    val rates = getMockResponse
    // UNSAFE, but is a quick test
    val head = Graph(rates)
    val currencies = Graph.currencies(head)
    currencies should contain allElementsOf allCurrencies
  }

  it should "return all the vertexes" in {
    val rates = getMockResponse
    // UNSAFE, but is a quick test
    val head = Graph(rates)
    val currenciesMap = Graph.vertexes(head)
    currenciesMap.size shouldBe 4 // We have 4 currencies on this test
    currenciesMap.keys should contain allElementsOf allCurrencies
    currenciesMap.forall { case (currency, graph) => graph.currency == currency } shouldBe true // All keys should match their node currency in the map
  }

  it should "get all the paths for a vertex" in {
    val rates = getMockResponse
    // UNSAFE, but is a quick test
    val head = Graph(rates) // The head is JPY here
    val paths = Graph.paths(head)
    paths.length shouldBe 16 // There are 16 possible paths for the first vertex of this graph
    val stringPaths = paths.map(path => showPath(path))
    stringPaths should contain allElementsOf List(
      "JPY-JPY",
      "JPY-BTC-EUR-USD-JPY",
      "JPY-BTC-EUR-JPY",
      "JPY-BTC-USD-EUR-JPY",
      "JPY-BTC-USD-JPY",
      "JPY-BTC-JPY",
      "JPY-EUR-BTC-USD-JPY",
      "JPY-EUR-BTC-JPY",
      "JPY-EUR-USD-BTC-JPY",
      "JPY-EUR-USD-JPY",
      "JPY-EUR-JPY",
      "JPY-USD-EUR-BTC-JPY",
      "JPY-USD-EUR-JPY",
      "JPY-USD-BTC-EUR-JPY",
      "JPY-USD-BTC-JPY",
      "JPY-USD-JPY"
    )
  }


  it should "return all the paths where there is arbitrage" in {
    val rates = getMockResponse
    // UNSAFE, but is a quick test
    val graph = Graph(rates) // The head is JPY here
    val paths = Graph.allNodesPaths(graph)
    paths.get(Currency("JPY")).map(_.length) shouldBe Some(16) // Every vertex has 16 different paths
    paths.get(Currency("BTC")).map(_.length) shouldBe Some(16) // Every vertex has 16 different paths
    paths.get(Currency("USD")).map(_.length) shouldBe Some(16) // Every vertex has 16 different paths
    paths.get(Currency("EUR")).map(_.length) shouldBe Some(16) // Every vertex has 16 different paths
    val arbitragePaths = Graph.getArbitragePaths(paths)
    arbitragePaths.length shouldBe 24 // There are 24 Arbitrage paths on this sample
    arbitragePaths.map(_._1).forall(_ > 1) shouldBe true // All should return a value greater than 1 (the exchange rate you have at the end of the complete trading)
  }

  it should "A path JPY-USD-EUR-BTC-JPY must have arbitrage with 1.031560172282602 in total" in {
    val rates = getMockResponse
    // UNSAFE, but is a quick test
    val graph = Graph(rates) // The head is JPY here
    val paths = Graph.allNodesPaths(graph)
    val arbitragePaths = Graph.getArbitragePaths(paths)
    val currencyPaths = arbitragePaths.map { case (d, path) => (d, path.map(_._2.currency)) }
    currencyPaths.find(_ == (1.031560172282602D, List(Currency("JPY"), Currency("USD"), Currency("EUR"), Currency("BTC"), Currency("JPY")))).map(_ => true) shouldBe Some(true)
  }

  it should "All arbitrage paths must be returned" in {
    val rates = getMockResponse
    // UNSAFE, but is a quick test
    val graph = Graph(rates) // The head is JPY here
    val paths = Graph.allNodesPaths(graph)
    val arbitragePaths = Graph.getArbitragePaths(paths)
    val strPaths = arbitragePaths.map(_._2).map(Graph.showPath)
    strPaths should contain allElementsOf List("EUR-JPY-EUR",
      "JPY-EUR-JPY",
      "USD-EUR-BTC-JPY-USD",
      "EUR-BTC-JPY-USD-EUR",
      "BTC-JPY-USD-EUR-BTC",
      "JPY-USD-EUR-BTC-JPY",
      "USD-JPY-EUR-BTC-USD",
      "EUR-BTC-USD-JPY-EUR",
      "BTC-USD-JPY-EUR-BTC",
      "JPY-EUR-BTC-USD-JPY",
      "EUR-JPY-BTC-EUR",
      "BTC-EUR-JPY-BTC",
      "JPY-BTC-EUR-JPY",
      "BTC-JPY-USD-BTC",
      "JPY-USD-BTC-JPY",
      "USD-BTC-JPY-USD",
      "USD-JPY-BTC-USD",
      "BTC-USD-JPY-BTC",
      "JPY-BTC-USD-JPY",
      "EUR-BTC-JPY-EUR",
      "BTC-JPY-EUR-BTC",
      "JPY-EUR-BTC-JPY",
      "BTC-JPY-BTC",
      "JPY-BTC-JPY"
    )
  }

}
