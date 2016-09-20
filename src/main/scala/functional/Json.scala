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
  type Parser[A, B] = State[A] => Xor[Error, State[B]]

  def parse(json: JsonString)(implicit converters: Converters = Map()): Xor[Error, Json] =
    if (json != null && !json.isEmpty)
      obj(State(json, Map())).map(data => new Json(data.data.asInstanceOf[Map[String, Any]]))
    else
      Left("Empty json")

  private def expr(state: State[Any])(implicit converters: Converters): Xor[Error, State[Any]] =
    value(state) orElse arr(state) orElse obj(state)

  private def value(state: State[Any])(implicit converters: Converters): Xor[Error, State[String]] = {
    val option = """^"(.+?)"""".r.findFirstMatchIn(state.jsonLeft)
    if (option.isDefined) {
      val valueString = option.get.group(1)
//      val value = converter(valueString)
      Right(State(state.jsonLeft.substring(option.get.group(0).length), valueString))
    } else
      Left(s"Expected something of the form 'value', found '${truncate(state.jsonLeft)}'")
  }

  private def arr(state: State[Any])(implicit converters: Converters): Xor[Error, State[List[Any]]] =
    for {
      s1 <- symbol("[", state)
      s2 <- repeat(expr, s1, ",")
      s3 <- symbol("]", s2)
    } yield s3

  private def obj(state: State[Any])(implicit converters: Converters): Xor[Error, State[Map[String, Any]]] =
    for {
      s1 <- symbol("{", state)
      s2 <- repeat(tuple, s1, ",")
      s3 <- symbol("}", s2)
    } yield mapEntriesToMap(s3)

  def mapEntriesToMap(state: State[Any]): State[Map[String, Any]] = {
    val list1: List[Any] = state.data.asInstanceOf[List[Any]]
    val map: List[(String, Any)] = list1.map((entry) => entry.asInstanceOf[(String, Any)])
    State(state.jsonLeft, map.toMap)
  }

  private def repeat(parser: Parser[Any, Any], state: State[Any], separator: String)(implicit converters: Converters): Xor[Error, State[List[Any]]] = {
    val first: Xor[Error, State[Any]] = parser(state)
    if (first.isLeft)
      Right(State(state.jsonLeft, List()))
    else {
      val s4 = for {
        s1 <- first
        s2 <- symbol(separator, s1)
        s3 <- repeat(parser, s2, separator)
      } yield State(s3.jsonLeft, first.toOption.get.data +: s3.data.asInstanceOf[List[Any]])
      if (s4.isRight) s4 else Right(State(first.toOption.get.jsonLeft, List(first.toOption.get.data)))
    }
  }

  private def tuple(state: State[Any])(implicit converters: Converters): Xor[Error, State[(String, Any)]] =
    for {
      s1 <- key(state)
      s2 <- symbol(":", s1)
      s3 <- expr(s2)
    } yield State(s3.jsonLeft, s1.data -> s3.data)

  private def key(state: State[Any]): Xor[Error, State[String]] = {
    val option = """^"(.+?)"""".r.findFirstMatchIn(state.jsonLeft)
    if (option.isDefined) {
      val text = option.get.group(1)
      Right(State(state.jsonLeft.substring(option.get.group(0).length), text))
    } else
      Left(s"Expected something of the form 'key', found '${truncate(state.jsonLeft)}'")
  }

  def symbol[A](symbol: String, state: State[A]): Xor[Error, State[A]] =
    if (state.jsonLeft.startsWith(symbol))
      Right(state.advance(1))
    else
      Left(s"Expected '$symbol', found '${state.jsonLeft}'")

  private def truncate(text: String) =
    text.substring(0, Math.min(text.length(), 30))

  case class State[+T](jsonLeft: JsonString, data: T) {
    def advance(chars: Int) =
      State(jsonLeft.substring(chars), data)

    def advance[U](chars: Int, newData: U) =
      State(jsonLeft.substring(chars), newData)
  }
}


