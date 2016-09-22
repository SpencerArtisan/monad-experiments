package functional

object OmdbQuery {
  type Query = String

  def searchFilms(title: String): Query =
    s"http://www.omdbapi.com/?s=${encode(title)}&type=movie&r=json"

  def getFilm(id: String): Query =
    s"http://www.omdbapi.com/?i=$id&type=movie&r=json"

  private def encode(text: String) =
    text.replace(' ', '+')
}
