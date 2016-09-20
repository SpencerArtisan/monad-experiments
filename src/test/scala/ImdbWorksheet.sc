import scalaz.Scalaz._
import scalaz._

import functional.{Json, OmdbQuery}
import scalaj.http.Http

val j = Json.parse("""{"hello":"world"}""")
val r1  = Http(OmdbQuery.film("Pygmalion")).asString.body
Json.parse(r1)

