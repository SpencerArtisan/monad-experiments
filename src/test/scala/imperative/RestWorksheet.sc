//import functional.{Json, OmdbQuery}
//import scalaj.http.Http
//
//val j = Json.parse("""{"hello":"world"}""")
//val r1  = Http(OmdbQuery.film("Pygmalion")).asString.body
//Json.parse(r1)

val text="[aaaa]"

def take(c: Char, str: String): Option[String] =
  if (str.head == c) {
    println(s"took $c")
    Some(str.tail)
  } else None


def as(str: String): Stream[String] = {
  val next = take('a', str)
  if (next.isDefined)
    next.get #:: as(next.get)
  else
    Stream.empty
}

val a = take('[', text)
val b = a.map((left) => as(left))

//def repeatTake(c: Char, str: String): List[String] = {
//  val res = take(c, str)
//  if (res.isEmpty)
//    List()
//  else
//    c :: repeatTake(c, res.get)
//}


//for (s1 <- take('[', text);
//     s2 <- repeatTake('a', s1);
//     s3 <- take(']', s2)) yield s3
