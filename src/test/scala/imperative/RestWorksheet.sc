import scalaz.{State, _}
import Scalaz._

//import functional.{Json, OmdbQuery}
//import scalaj.http.Http
//
//val j = Json.parse("""{"hello":"world"}""")
//val r1  = Http(OmdbQuery.film("Pygmalion")).asString.body
//Json.parse(r1)

type LeftOver = String
type Parser[T] = LeftOver => Result[T]
type Result[T] = Option[(LeftOver, T)]

def parse(input: String): Option[List[Any]] =
  array(input).map(_._2)

def expr(input: LeftOver): Result[Any] =
  value(input) orElse array(input)

def value(input: LeftOver): Result[Int] =
  input.head.toString.parseInt match {
    case Success(i) => (input.tail, i).some
    case Failure(e) => None
  }

def array(input: LeftOver): Result[List[Any]] =
  for {
    s1 <- symbol('[', input)
    s2 <- repeat(s1._1)(expr, ',')
    s3 <- symbol(']', s2._1)
  } yield (s3._1, s2._2)

def symbol(symbol: Char, input: LeftOver): Result[Null] =
  (input.head == symbol) ? (input.tail, null).some | None

def repeat(input: LeftOver)(implicit parser: Parser[Any], delimiter: Char): Result[List[Any]] = {
  val first: Option[(LeftOver, Any)] = parser(input)
  if (first.isEmpty)
    (input, List()).some
  else {
    for {
        s2 <- symbol(',', first.get._1)
        s3 <- repeat(s2._1)
      } yield (s3._1, first.get._2 +: s3._2)
  } orElse (first.get._1, List(first.get._2)).some
}

println(parse("1"))
println(parse("1junk"))
println(parse("junk"))
println(parse("[]"))
println(parse("[[]]"))
println(parse("[1]"))
println(parse("[1,2]"))
println(parse("[1,2,3]"))
println(parse("[[],[2,3]]"))

