package functional

import cats.data.Xor
import cats.data.Xor._

class Json(private val data: Map[String, Any]) {
  def apply(key: String): Option[Any] =
    if (data.contains(key)) Some(data(key)) else None

  override def toString: String =
    data.mkString("{\n", "\n", "\n}")
}

object Json {
  type Error = String
  type JsonString = String
  type Converter[T] = String => T
  type Converters = Map[String, Converter[_]]
  type Parser[T] = T => Xor[Error, T]



  def parse(json: JsonString)(implicit converters: Converters = Map()): Xor[Error, Json] =
    if (json != null && !json.isEmpty)
      element(MapState(json, Map())).map(_.toJson)
    else
      Left("Empty json")

  private def element(state: MapState)(implicit converters: Converters): Xor[Error, MapState] =
    for (s1 <- symbol("{", state);
         s2 <- repeat(keyValuePair, s1, ",");
         s3 <- symbol("}", s2)) yield s3

  private def list(state: ListState)(implicit converters: Converters): Xor[Error, ListState] =
    for (s1 <- symbol("[", state);
//         s1 <- element(new MapState(s1.jsonLeft, Map()), ",");
         s3 <- symbol("]", s1)) yield s3

  private def repeat[A <: State[A]](parser: Parser[A], state: A, separator: String)(implicit converters: Converters): Xor[Error, A] = {
    val newState = parser(state)
    if (newState.isLeft)
      Right(state)
    else {
      val s4 = for (s1 <- newState;
                    s2 <- symbol(separator, s1);
                    s3 <- repeat(parser, s2, separator)) yield s3
      if (s4.isRight) s4 else newState
    }
  }

  private def keyValuePair(state: MapState)(implicit converters: Converters): Xor[Error, MapState] = {
    val keySimpleValue = """"(.+?)":"""".r.findFirstMatchIn(state.jsonLeft)
    val keyListValue = """"(.+?)":\[""".r.findFirstMatchIn(state.jsonLeft)

    if (keySimpleValue.isDefined)
      keySimplePair(state)
    else if (keyListValue.isDefined)
      keyListPair(state)
    else
      Left(s"Expected something of the form key:value, found '${truncate(state.jsonLeft)}'")
  }

  private def keySimplePair(state: MapState)(implicit converters: Converters): Xor[Error, MapState] = {
    val option = """"(.+?)":"(.+?)"""".r.findFirstMatchIn(state.jsonLeft)

    if (option.isDefined) {
      val key = option.get.group(1)
      val valueString = option.get.group(2)
      val value = if (converters.contains(key)) converters(key).apply(valueString) else valueString
      Right(state.advance(option.get.group(0).length, key -> value))
    } else
      Left(s"Expected something of the form key:value, found '${truncate(state.jsonLeft)}'")
  }

  private def keyListPair(state: MapState)(implicit converters: Converters): Xor[Error, MapState] = {
    val option = """"(.+?)":""".r.findFirstMatchIn(state.jsonLeft)

    if (option.isDefined) {
      val key = option.get.group(1)
      val nestedState = state.nested(option.get.group(0).length)
      for (s1 <- list(nestedState);
           s2 = new MapState(s1.jsonLeft, state.data + (key -> nestedState.data))) yield s2
    } else
      Left(s"Expected something of the form key:value, found '${truncate(state.jsonLeft)}'")
  }

  def symbol[A <: State[A]](symbol: String, state: A): Xor[Error, A] =
    if (state.jsonLeft.startsWith(symbol))
      Right(state.advance(1))
    else
      Left(s"Expected '$symbol', found '${state.jsonLeft}'")

  private def truncate(text: String) =
    text.substring(0, Math.min(text.length(), 30))

  trait State[A] {
    def advance(chars: Int): A
    def jsonLeft: JsonString
  }

  case class MapState(jsonLeft: JsonString, data: Map[String, Any]) extends State[MapState] {
    def advance(chars: Int): MapState =
      MapState(jsonLeft.substring(chars), data)

    def advance(chars: Int, newData: (String, Any)) =
      MapState(jsonLeft.substring(chars), data + newData)

    def nested(chars: Int) =
      ListState(jsonLeft.substring(chars), List())

    def toJson = new Json(data)
  }

  case class ListState(jsonLeft: JsonString, data: List[Any]) extends State[ListState]  {
    def advance(chars: Int) =
      ListState(jsonLeft.substring(chars), data)
  }

  object MapState {
    implicit def toJson(state: MapState): Json = new Json(state.data)
  }
}


