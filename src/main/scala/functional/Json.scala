package functional

import scalaz._
import Scalaz._

class Json(private val data: Map[String, Any]) {
  def apply(key: String): Option[Any] =
    if (data.contains(key)) data(key).some else None

  override def toString: String =
    data.mkString("{\n", "\n", "\n}")
}

object Json {
  type Error = String
  type JsonString = String
  type Converter[T] = String => T
  type Converters = Map[String, Converter[_]]
  type Parser[A, B] = State[A] => Option[State[B]]

  def parse(json: JsonString)(implicit converters: Converters = Map()): Option[Json] =
    if (json != null && !json.isEmpty)
      obj(State(json, Map())).map(state => new Json(state.data))
    else
      None

  private def expr(state: State[Any])(implicit converters: Converters): Option[State[Any]] =
    text(state) orElse arr(state) orElse obj(state)

  private def text(state: State[Any])(implicit converters: Converters): Option[State[String]] = {
    val option = """^"(.+?)"""".r.findFirstMatchIn(state.jsonLeft)
    if (option.isDefined) {
      val valueString = option.get.group(1)
//      val value = converter(valueString)
      State(state.jsonLeft.substring(option.get.group(0).length), valueString).some
    } else
      None
  }

  private def arr(state: State[Any])(implicit converters: Converters): Option[State[List[Any]]] =
    for {
      s1 <- symbol("[", state)
      s2 <- repeat(expr, s1, ",")
      s3 <- symbol("]", s2)
    } yield s3

  private def obj(state: State[Any])(implicit converters: Converters): Option[State[Map[String, Any]]] =
    for {
      s1 <- symbol("{", state)
      s2 <- repeat(tuple, s1, ",")
      s3 <- symbol("}", s2)
    } yield State(s3.jsonLeft, s3.data.toMap)

  private def repeat[B <: Any](parser: Parser[Any, B], state: State[Any], separator: String)(implicit converters: Converters): Option[State[List[B]]] = {
    val first = parser(state)
    if (first.isEmpty)
      State(state.jsonLeft, List()).some
    else {
      for {
          s1 <- first
          s2 <- symbol(separator, s1)
          s3 <- repeat(parser, s2, separator)
        } yield State(s3.jsonLeft, first.get.data +: s3.data)
    } orElse first.map { s => State(s.jsonLeft, List(s.data)) }
  }

  private def tuple(state: State[Any])(implicit converters: Converters): Option[State[(String, Any)]] =
    for {
      s1 <- text(state)
      s2 <- symbol(":", s1)
      s3 <- expr(s2)
    } yield State(s3.jsonLeft, s1.data -> s3.data)

  def symbol[A](symbol: String, state: State[A]): Option[State[A]] =
    if (state.jsonLeft.startsWith(symbol))
      state.advance(1).some
    else
      None

  private def truncate(text: String) =
    text.substring(0, Math.min(text.length(), 30))

  case class State[+T](jsonLeft: JsonString, data: T) {
    def advance(chars: Int) =
      State(jsonLeft.substring(chars), data)

    def advance[U](chars: Int, newData: U) =
      State(jsonLeft.substring(chars), newData)
  }
}


