package functional

object OmdbQuery {
  type Query = String

  def film(title: String): Query = {
    s"http://www.omdbapi.com/?s=${encode(title)}&type=movie&r=json"
  }

  private def encode(text: String) = {
    text.replace(' ', '+')
  }
}
