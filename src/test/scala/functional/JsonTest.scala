package functional

import org.scalatest._
import org.scalatest.mockito.MockitoSugar
import functional.Json._

class JsonTest extends FunSpec with Inside with Matchers with MockitoSugar {
  it("should not parse empty string") {
    Json.parse("").isEmpty should be (true)
  }

  it("should not parse null") {
    Json.parse(null).isEmpty should be (true)
  }

  it("should not parse non-json") {
    Json.parse("invalid").isEmpty should be (true)
  }

  it("should parse empty json") {
    val json = Json.parse("{}")
    json > "key" should be (Json(None))
  }

  it("should parse json value") {
    val json = Json.parse("""{"key":"value"}""")
    json > "key" should be (Json(Some("value")))
  }

  it("should parse empty json array") {
    val json = Json.parse("""{"key":[]}""")
    json > "key" should be (Json(Some(List())))
  }

  it("should parse json array with empty object") {
    val json = Json.parse("""{"key":[{}]}""")
    json > "key" should be (Json(Some(List(Map()))))
  }

  it("should parse json array with non-empty values") {
    val json = Json.parse("""{"key":["1"]}""")
    json > "key" should be (Json(Some(List("1"))))
  }

  it("should parse json array with non-empty objects") {
    val json = Json.parse("""{"key":[{"key2":"value"}]}""")
    json > "key" should be (Json(Some(List(Map("key2" -> "value")))))
  }

  it("should parse json value with spaces") {
    val json = Json.parse("""{"key":"a value"}""")
    json > "key" should be (Json(Some("a value")))
  }

  it("should parse two json values") {
    val json = Json.parse("""{"key1":"value1","key2":"value2"}""")
    json > "key1" should be (Json(Some("value1")))
    json > "key2" should be (Json(Some("value2")))
  }

  it("should parse multiple json values") {
    val json = Json.parse("""{"key1":"value1","key2":"value2","key3":"value3"}""")
    json > "key1" should be (Json(Some("value1")))
    json > "key2" should be (Json(Some("value2")))
    json > "key3" should be (Json(Some("value3")))
  }

  it("should use a custom value converter") {
    val json = Json.parse("""{"key":"42"}""")(new Converter(Map("key" -> ((s:String) => s.toInt))))
    json > "key" should be (Json(Some(42)))
  }

  it("should use a custom value converter 2") {
    val json = Json.parse("""{"key":"hello"}""")(new Converter(Map("key" -> ((s:String) => s.toUpperCase))))
    json > "key" should be (Json(Some("HELLO")))
  }
}