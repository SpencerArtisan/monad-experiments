package functional

import java.io.File

import org.scalatest._
import org.scalatest.mockito.MockitoSugar
import functional.Json._

import scala.io.Source

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

  it("should parse json quoted value") {
    val json = Json.parse("""{"key":"value"}""")
    (json > "key" toValue) should be ("value")
  }

  it("should parse json true boolean value") {
    val json = Json.parse("""{"key":true}""")
    json > "key" should be (Json(Some(true)))
  }

  it("should parse json false boolean value") {
    val json = Json.parse("""{"key":false}""")
    json > "key" should be (Json(Some(false)))
  }

  it("should parse json integer value") {
    val json = Json.parse("""{"key":42}""")
    json > "key" should be (Json(Some(42)))
  }

  it("should parse json decimal value") {
    val json = Json.parse("""{"key":12.34}""")
    json > "key" should be (Json(Some(12.34)))
  }

  it("should parse json negative integer value") {
    val json = Json.parse("""{"key":-42}""")
    json > "key" should be (Json(Some(-42)))
  }

  it("should parse json negative decimal value") {
    val json = Json.parse("""{"key":-12.34}""")
    json > "key" should be (Json(Some(-12.34)))
  }

  it("should parse json null value") {
    val json = Json.parse("""{"key":null,"key2":42}""")
    json > "key" should be (Json(Some(null)))
    json > "key2" should be (Json(Some(42)))
  }

  it("should parse empty json array") {
    val json = Json.parse("[]")
    json should be (Json(Some(List())))
  }

  it("should parse empty json array inside an object") {
    val json = Json.parse("""{"key":[]}""")
    json > "key" should be (Json(Some(List())))
  }

  it("should parse json array with empty object") {
    val json = Json.parse("[{}]")
    json should be (Json(Some(List(Map()))))
  }

  it("should parse json array with empty object inside an object") {
    val json = Json.parse("""{"key":[{}]}""")
    json > "key" should be (Json(Some(List(Map()))))
  }

  it("should parse json array with non-empty values") {
    val json = Json.parse("[1]")
    json should be (Json(Some(List(1))))
  }

  it("should parse json array with multiple non-empty values") {
    val json = Json.parse("[1, 2, 3]")
    json should be (Json(Some(List(1, 2, 3))))
  }

  it("should parse json array with non-empty values inside an object") {
    val json = Json.parse("""{"key":["1"]}""")
    json > "key" should be (Json(Some(List("1"))))
  }

  it("should parse json array with multiple non-empty values inside an object") {
    val json = Json.parse("""{"key":[1, 2, 3]}""")
    json > "key" should be (Json(Some(List(1, 2, 3))))
  }

  it("should parse json array with non-empty objects") {
    val json = Json.parse("""[{"key2":"value"}]""")
    json should be (Json(Some(List(Map("key2" -> "value")))))
  }

  it("should parse json nested array with non-empty objects") {
    val json = Json.parse("""{"key":[{"key2":"value"}]}""")
    json > "key" should be (Json(Some(List(Map("key2" -> "value")))))
  }

  it("should parse json with whitespace") {
    val json = Json.parse("""{  "key": [ { "key2" : "value" } ] }""")
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
//
//  it("should use a custom value converter") {
//    val json = Json.parse("""{"key":"42"}""")(new Converter(Map("key" -> ((s:String) => s.toInt))))
//    json > "key" should be (Json(Some(42)))
//  }
//
//  it("should use a custom value converter 2") {
//    val json = Json.parse("""{"key":"hello"}""")(new Converter(Map("key" -> ((s:String) => s.toUpperCase))))
//    json > "key" should be (Json(Some("HELLO")))
//  }

  it("should handle complex json 1") {
    val json = testFromFile("example1.json")
    json > "colorsArray" > 0 > "colorName" should be (Json(Some("red")))
  }

  it("should handle complex json 2") {
    val json = testFromFile("example2.json")
    json > "isAlive" should be (Json(Some(true)))
  }

  it("should handle complex json 3") {
    val json = testFromFile("example3.json")
    json > "ppu" should be (Json(Some(0.55)))
  }

  it("should handle complex json 4") {
    val json = testFromFile("example4.json")
    json > 2 > "batters" > "batter" > 1 > "id" should be (Json(Some("1002")))
  }

  it("should handle complex json 5") {
    val json = testFromFile("example5.json")
    json > "properties" > "storage" > "oneOf" > 1 > "$ref" should be (Json(Some("#/definitions/diskUUID")))
  }

  it("should handle complex json 6") {
    val json = testFromFile("example6.json")
    json > "definitions" > "diskDevice" > "properties" > "device" > "pattern" should be (Json(Some("^/dev/[^/]+(/[^/]+)*$")))
    json > "definitions" > "diskDevice" > "additionalProperties" should be (Json(Some(false)))
  }

  it("should handle complex json 7") {
    val json = testFromFile("example7.json")
    json > "result_count" should be (Json(Some(5)))
  }

  private def testFromFile(file: String): Json = {
    val json = Source.fromURL(getClass.getResource("/" + file)).mkString
    Json.parse(json)
  }
}