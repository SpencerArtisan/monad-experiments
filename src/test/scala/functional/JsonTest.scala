package functional

import org.scalatest._
import org.scalatest.mockito.MockitoSugar

import functional.Json._

class JsonTest extends FunSpec with Inside with Matchers with MockitoSugar {
  it("should not parse empty string") {
    Json.parse("").isLeft should be (true)
  }

  it("should not parse null") {
    Json.parse(null).isLeft should be (true)
  }

  it("should not parse non-json") {
    Json.parse("invalid").isLeft should be (true)
  }

  it("should parse empty json") {
    val json = Json.parse("{}")
    json.toOption.get("key") should be (None)
  }

  it("should parse json value") {
    val json = Json.parse("""{"key":"value"}""")
    json.toOption.get("key") should be (Some("value"))
  }

  it("should parse empty json array") {
    val json = Json.parse("""{"key":[]}""")
    json.toOption.get("key") should be (Some(List()))
  }

  it("should parse json array with empty object") {
    val json = Json.parse("""{"key":[{}]}""")
    json.toOption.get("key").get should be (Some(Map()))
  }

  it("should parse json array with non-empty object") {
    val json = Json.parse("""{"key":[{"key2":"value"}]}""")
    json.toOption.get("key").get should be (Some(Map("key2" -> "value")))
  }

  it("should parse json value with spaces") {
    val json = Json.parse("""{"key":"a value"}""")
    json.toOption.get("key") should be (Some("a value"))
  }

  it("should parse two json values") {
    val json = Json.parse("""{"key1":"value1","key2":"value2"}""")
    json.toOption.get.apply("key1") should be (Some("value1"))
    json.toOption.get.apply("key2") should be (Some("value2"))
  }

  it("should parse multiple json values") {
    val json = Json.parse("""{"key1":"value1","key2":"value2","key3":"value3"}""")
    json.toOption.get.apply("key1") should be (Some("value1"))
    json.toOption.get.apply("key2") should be (Some("value2"))
    json.toOption.get.apply("key3") should be (Some("value3"))
  }

  it("should use a custom value converter") {
    val json = Json.parse("""{"key":"42"}""")(Map("key" -> ((s:String) => s.toInt)))
    json.toOption.get("key") should be (Some(42))
  }

  it("should use a custom value converter 2") {
    val json = Json.parse("""{"key":"hello"}""")(Map("key" -> ((s:String) => s.toUpperCase)))
    json.toOption.get("key") should be (Some("HELLO"))
  }
}