package functional

import org.scalatest._
import org.scalatest.mockito.MockitoSugar

class OmdbQueryTest extends FunSpec with Inside with Matchers with MockitoSugar {
  it("should create a film query") {
    OmdbQuery.searchFilms("title") should be ("http://www.omdbapi.com/?s=title&type=movie&r=json")
  }

  it("should create a film query with spaces") {
    OmdbQuery.searchFilms("a title") should be ("http://www.omdbapi.com/?s=a+title&type=movie&r=json")
  }
}