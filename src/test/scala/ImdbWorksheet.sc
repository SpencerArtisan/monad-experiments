import functional.{Json, OmdbQuery}

import scalaj.http.Http

val j = Json.parse("""{"hello":"world"}""")
val r1  = Http(OmdbQuery.searchFilms("Pygmalion")).asString.body
val json = Json.parse(r1)

val id: Option[String] = json > "Search" > 0 > "imdbID"

val r2  = Http(OmdbQuery.getFilm(id.get)).asString.body
Json.parse(r2).get.toString


