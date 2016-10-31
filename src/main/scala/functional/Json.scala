package functional

import scalaz._
import Scalaz._
import scala.util.matching.Regex.Match

case class Json(data: Option[Any]) {
  def >(key: String): Json =
    Json(data flatMap { _.asInstanceOf[Map[String, Any]].get(key) })

  def >(index: Int): Json =
    Json(data flatMap { _.asInstanceOf[List[Any]].lift(index) })

  def toValue: Any = data.get
}

object Json {
  type JsonString = String
  type ElementParser[A, B] = State[A] => Option[State[B]]

  implicit def jsonToOption[T](json: Json): Option[T] = json.data map { _.asInstanceOf[T] }

  def parse(jsonText: JsonString)(implicit converter: Converter = new Converter()): Json =
    Json(for {
      json <- Option(jsonText)
      objState <- value(State(json))
    } yield objState.data)

  private def value(state: State[Any])(implicit converter: Converter): Option[State[Any]] =
    obj(state) orElse
    arr(state) orElse
    stringValue(state) orElse
    booleanValue(state) orElse
    doubleValue(state) orElse
    intValue(state) orElse
    nullValue(state)

  private def intValue(state: State[Any]): Option[State[Any]] =
    regExValue("""-?\d+""", v => v.toString.toInt, state)

  private def doubleValue(state: State[Any]): Option[State[Any]] =
    regExValue("""-?\d+\.\d+""", v => v.toString.toDouble, state)

  private def booleanValue(state: State[Any]): Option[State[Any]] =
    trueValue(state) orElse falseValue(state)

  private def trueValue(state: State[Any]): Option[State[Any]] =
    regExValue("true", _ => true, state)

  private def falseValue(state: State[Any]): Option[State[Any]] =
    regExValue("false", _ => false, state)

  private def nullValue(state: State[Any]): Option[State[Any]] =
    regExValue("null", _ => null, state)

  private def stringValue[B](state: State[Any])(implicit converter: Converter): Option[State[B]] =
    matchRegEx("\"(.+?)\"", state).map(m => state.advance(m.group(0).length, converter.convert(m.group(1))))

  private def regExValue[B](pattern: String, result: String => B, state: State[Any]): Option[State[B]] =
    matchRegEx(pattern, state).map(m => state.advance(m.group(0).length, result(m.group(m.groupCount))))

  private def matchRegEx[B](pattern: String, state: State[Any]): Option[Match] =
    ("""^""" + pattern).r.findFirstMatchIn(state.json)

  private def elements(state: State[Any])(implicit converter: Converter): Option[State[List[Any]]] =
    valueCommaThenElements(state) orElse value(state).map(s => s.mapData(d => List(d)))

  private def emptyArray(state: State[Any])(implicit converter: Converter): Option[State[List[Any]]] =
    regExValue("\\[\\]", _ => List(), state)

  private def valueCommaThenElements(state: State[Any])(implicit converter: Converter): Option[State[List[Any]]] =
    for {
      valueState <- value(state)
      commaState <- symbol(",", valueState)
      elementsState <- elements(commaState)
    } yield elementsState.mapData { valueState.data +: _ }

  private def arr(state: State[Any])(implicit converter: Converter): Option[State[List[Any]]] =
    emptyArray(state) orElse arrXXX(state)

  private def arrXXX(state: State[Any])(implicit converter: Converter): Option[State[List[Any]]] =
    for {
      openBracketState <- symbol("[", state)
      elementsState <- elements(openBracketState)
      closeBracketState <- symbol("]", elementsState)
    } yield closeBracketState

  private def obj(state: State[Any])(implicit converter: Converter): Option[State[Map[String, Any]]] =
    for {
      openBraceState <- symbol("{", state)
      elementsState <- repeat(tuple, openBraceState, ",")
      closeBraceState <- symbol("}", elementsState)
    } yield closeBraceState.mapData { _.toMap }

  private def repeat[B <: Any](parser: ElementParser[Any, B], state: State[Any], separator: String)(implicit converter: Converter): Option[State[List[B]]] = {
    val firstElementStateOption = parser(state)
    if (firstElementStateOption.isEmpty)
      state.newData(List()).some
    else {
      for {
        firstElementState <- firstElementStateOption
        separatorState <- symbol(separator, firstElementState)
        restElementsState <- repeat(parser, separatorState, separator)
      } yield restElementsState.mapData { firstElementState.data +: _ }
    } orElse firstElementStateOption.map { _.mapData { List(_) } }
  }

  private def tuple(state: State[Any])(implicit converter: Converter): Option[State[(String, Any)]] =
    for {
      firstState <- stringValue[String](state)
      colonState <- symbol(":", firstState)
      secondState <- value(colonState)(converter.withDefault(firstState.data))
    } yield secondState.mapData(firstState.data -> _)

  private def symbol[A](symbol: String, state: State[A]): Option[State[A]] =
    state.json.startsWith(symbol).option(state.advance(symbol.length))


  case class State[+T](private val jsonLeft: JsonString, data: T = null) {
    val json: JsonString = jsonLeft.replaceAll("^\\s+", "")

    def advance(chars: Int) =
      State(json.substring(chars), data)

    def advance[U](chars: Int, newData: U) =
      State(json.substring(chars), newData)

    def newData[U](newData: U) =
      State(json, newData)

    def mapData[U](f: T => U) =
      State(json, f(data))
  }

  class Converter(converters: Map[String, String => Any] = Map(), defaultConverter: String => Any = _.toString) {
    def convert[B](a: String): B = defaultConverter(a).asInstanceOf[B]
    def withDefault(f: String => Any) = new Converter(converters, f)
    def withDefault(key: String) = converters.contains(key) ? new Converter(converters, converters(key)) | this
  }
}


