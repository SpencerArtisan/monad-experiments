package functional

import scalaz._
import Scalaz._
import scala.util.matching.Regex.Match

case class Json(data: Option[Any]) {
  def >(key: String): Json =
    Json(data flatMap { _.asInstanceOf[Map[String, Any]].get(key) })

  def >(index: Int): Json =
    Json(data flatMap { _.asInstanceOf[List[Any]].lift(index) })
}

object Json {
  type JsonString = String
  type ElementParser[A, B] = State[A] => Option[State[B]]

  implicit def jsonToOption[T](json: Json): Option[T] = json.data map { _.asInstanceOf[T] }

  def parse(jsonText: JsonString)(implicit converter: Converter = new Converter()): Json =
    Json(for {
      json <- Option(jsonText)
      objState <- expr(State(json))
    } yield objState.data)

  private def expr(state: State[Any])(implicit converter: Converter): Option[State[Any]] =
    valueZ(state) orElse arr(state) orElse obj(state)

  private def valueZ(state: State[Any])(implicit converter: Converter): Option[State[Any]] =
    quotedValue(state) orElse booleanValue(state)

  private def booleanValue(state: State[Any])(implicit converter: Converter): Option[State[Any]] =
    trueValue(state) orElse falseValue(state)

  private def trueValue(state: State[Any])(implicit converter: Converter): Option[State[Any]] =
    regEx("true", true, state)

  private def falseValue(state: State[Any])(implicit converter: Converter): Option[State[Any]] =
    regEx("false", false, state)

  private def regEx(pattern: String, result: Any, state: State[Any]): Option[State[Any]] =
    ("""^""" + pattern).r.findFirstMatchIn(state.json).map(m => state.advance(m.group(0).length, result))

  private def quotedValue[B](state: State[Any])(implicit converter: Converter): Option[State[B]] =
    quoted(state).map { m => state.advance(m.group(0).length, converter.convert(m.group(1))) }

  private def arr(state: State[Any])(implicit converter: Converter): Option[State[List[Any]]] =
    for {
      openBracketState <- symbol("[", state)
      elementsState <- repeat(expr, openBracketState, ",")
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

  private def quoted(state: State[Any]): Option[Match] =
    """^"(.+?)"""".r.findFirstMatchIn(state.json)

  private def tuple(state: State[Any])(implicit converter: Converter): Option[State[(String, Any)]] =
    for {
      firstState <- quotedValue[String](state)
      colonState <- symbol(":", firstState)
      secondState <- expr(colonState)(converter.withDefault(firstState.data))
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


