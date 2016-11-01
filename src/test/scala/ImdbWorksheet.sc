import functional.{Json, OmdbQuery}

import scalaj.http.Http

val j = Json.parse("""{"hello":"world"}""")
val r1  = Http(OmdbQuery.searchFilms("Pygmalion")).asString.body
val json = Json.parse(r1)

val id: Option[String] = json > "Search" > 0 > "imdbID"

val r2  = Http(OmdbQuery.getFilm(id.get)).asString.body
Json.parse(r2).get.toString


minusTen(13)
minusNine(13)
minusY(11, 1)
def minusTen(x: Int): Int = x - 10
def minusNine: Int => Int = _ - 9
def minusY(x: Int, y: Int) = x - y