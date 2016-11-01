package functional

import scalaz._
import Scalaz._
import scala.collection.immutable.Stack
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

  def parse(jsonText: JsonString): Json =
    Json(for {
      json <- Option(jsonText)
      objState <- value(State(json))
    } yield objState.singleValue)

  private def value(state: State[Any]): Option[State[Any]] =
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

  private def stringValue(state: State[Any]): Option[State[Any]] =
    matchRegEx("\"(.+?)\"", state).map(m => state.advance(m.group(0).length, m.group(1)))

  private def regExValue[B](pattern: String, result: String => B, state: State[Any]): Option[State[B]] =
    matchRegEx(pattern, state).map(m => state.advance(m.group(0).length, result(m.group(m.groupCount))))

  private def matchRegEx(pattern: String, state: State[Any]): Option[Match] =
    ("""^""" + pattern).r.findFirstMatchIn(state.json)

  private def arr(state: State[Any]): Option[State[List[Any]]] =
    emptyArray(state) orElse nonEmptyArray(state)

  private def emptyArray(state: State[Any]): Option[State[List[Any]]] =
    regExValue("\\[\\]", _ => List(), state)

  private def nonEmptyArray(state: State[Any]): Option[State[List[Any]]] =
    Some(state) flatMap symbol("[") flatMap elements flatMap symbol("]") map asList

  private def elements(state: State[Any]): Option[State[Any]] =
    valueCommaThenElements(state) orElse value(state)

  private def valueCommaThenElements(state: State[Any]): Option[State[Any]] =
    Some(state) flatMap value flatMap ignoredSymbol(",") flatMap elements

  private def obj(state: State[Any]): Option[State[Map[String, Any]]] =
    emptyObj(state) orElse nonEmptyObj(state)

  private def emptyObj(state: State[Any]): Option[State[Map[String, Any]]] =
    regExValue("\\{\\}", _ => Map[String, Any](), state)

  private def nonEmptyObj(state: State[Any]): Option[State[Map[String, Any]]] =
    Some(state) flatMap symbol("{") flatMap members flatMap symbol("}") map asMap

  private def members(state: State[Any]): Option[State[Any]] =
    tupleCommaThenMembers(state) orElse tuple(state)

  private def tupleCommaThenMembers(state: State[Any]): Option[State[Any]] =
    tuple(state) flatMap ignoredSymbol(",") flatMap members

  private def tuple(state: State[Any]): Option[State[Any]] =
    Some(state) flatMap stringValue flatMap ignoredSymbol(":") flatMap value map asTuple

  private def symbol(symbol: String)(state: State[Any]): Option[State[Any]] =
    state.json.startsWith(symbol).option(state.advance(symbol.length, symbol))

  private def ignoredSymbol(symbol: String)(state: State[Any]): Option[State[Any]] =
    state.json.startsWith(symbol).option(state.advance(symbol.length))

  private def asTuple[A](state: State[Any]): State[(String, Any)] =
    state.popTuple()

  private def asMap[A](state: State[Any]): State[Map[String, Any]] =
    state.popTo("{").mapHead(a => a.asInstanceOf[List[(String, Any)]].toMap)

  private def asList[A](state: State[Any]): State[List[Any]] =
    state.popTo("[")

  case class State[+T](private val jsonLeft: JsonString, stack: List[Any] = List()) {
    val json: JsonString = jsonLeft.replaceAll("^\\s+", "")

    def advance(chars: Int): State[T] =
      State(json.substring(chars), stack)

    def advance[U](chars: Int, newData: U): State[U] =
      State(json.substring(chars), newData :: stack)

    def push[U](newData: U): State[U] =
      advance(0, newData)

    def popTo(marker: Any): State[List[Any]] =
      State(json, stack.drop(1).takeWhile(item => item != marker).reverse :: stack.dropWhile(item => item != marker).drop(1))

    def popTuple(marker: Any): State[(String, Any)] =
      State(json, (stack.tail.head, stack.head) :: stack.drop(2))

    def singleValue: T =
      stack.head.asInstanceOf[T]

    def mapHead[U](f: T => U): State[U] =
      State(json, f(stack.head.asInstanceOf[T]) :: stack.tail)
  }
}


