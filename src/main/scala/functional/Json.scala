package functional

import scalaz._
import Scalaz._

case class Json(private val data: Option[Any]) {
  def >(key: String): Json =
    Json(data flatMap { _.asInstanceOf[Map[String, Any]].get(key) })

  def >(index: Int): Json =
    Json(data flatMap { _.asInstanceOf[List[Any]].lift(index) })

  def get: Any = data.get
}

object Json {
  type Parser[T] = State[Any] => Option[State[T]]

  implicit def jsonToOption[T](json: Json): Option[T] = json.data map { _.asInstanceOf[T] }
  implicit def stateToOption[T](state: State[T]): Option[State[T]] = Option(state)
  implicit class OptionPlus[A](o: Option[A]) {
    def |[B >: A](alternative: => Option[B]): Option[B] =
      o.orElse(alternative)
  }

  def parse(jsonText: String): Json =
    Json(for {
      json <- Option(jsonText)
      objState <- value(State(json))
    } yield objState.value)

  private def value: Parser[Any] = (state) =>
    obj(state) |
    arr(state) |
    stringValue(state) |
    booleanValue(state) |
    numberValue(state) |
    nullValue(state)

  private def numberValue: Parser[Any] = (state) =>
    doubleValue(state) | intValue(state)

  private def intValue: Parser[Any] =
    regExValue("""-?\d+""", _.toInt)

  private def doubleValue: Parser[Any] =
    regExValue("""-?\d+\.\d+""", _.toDouble)

  private def booleanValue: Parser[Any] = (state) =>
    trueValue(state) | falseValue(state)

  private def trueValue: Parser[Any] =
    regExValue("true", _ => true)

  private def falseValue: Parser[Any] =
    regExValue("false", _ => false)

  private def nullValue: Parser[Any] =
    regExValue("null", _ => null)

  private def stringValue: Parser[Any] =
    regExValue("\"(.+?)\"", identity)

  private def arr: Parser[List[Any]] = (state) =>
    emptyArray(state) | nonEmptyArray(state)

  private def emptyArray: Parser[List[Any]] =
    regExValue("\\[\\]", _ => List())

  private def nonEmptyArray: Parser[List[Any]] = (state) =>
    state flatMap symbol("[") flatMap elements flatMap symbol("]") map asList

  private def elements: Parser[Any] = (state) =>
    valueCommaThenElements(state) | value(state)

  private def valueCommaThenElements: Parser[Any] = (state) =>
    Some(state) flatMap value flatMap ignoredSymbol(",") flatMap elements

  private def obj: Parser[Map[String, Any]] = (state) =>
    emptyObj(state) | nonEmptyObj(state)

  private def emptyObj: Parser[Map[String, Any]] =
    regExValue("\\{\\}", _ => Map[String, Any]())

  private def nonEmptyObj: Parser[Map[String, Any]] = (state) =>
    state flatMap symbol("{") flatMap members flatMap symbol("}") map asMap

  private def members(state: State[Any]): Option[State[Any]] =
    tupleCommaThenMembers(state) | tuple(state)

  private def tupleCommaThenMembers: Parser[Any] = (state) =>
    tuple(state) flatMap ignoredSymbol(",") flatMap members

  private def tuple: Parser[Any] = (state) =>
    state flatMap stringValue flatMap ignoredSymbol(":") flatMap value map asTuple

  private def symbol(symbol: String): Parser[Any] = (state) =>
    state.json.startsWith(symbol).option(state.advance(symbol.length, symbol))

  private def ignoredSymbol(symbol: String): Parser[Any] = (state) =>
    state.json.startsWith(symbol).option(state.advance(symbol.length))

  private def asTuple[A](state: State[Any]): State[(String, Any)] =
    state.popTuple()

  private def asMap[A](state: State[Any]): State[Map[String, Any]] =
    state.popTo("{").map(a => a.asInstanceOf[List[(String, Any)]].toMap)

  private def asList[A](state: State[Any]): State[List[Any]] =
    state.popTo("[")

  private def regExValue[B](pattern: String, result: String => B): Parser[B] = (state) =>
    ("^" + pattern).r.findFirstMatchIn(state.json).map(m => state.advance(m.group(0).length, result(m.group(m.groupCount))))

  case class State[+T](private val jsonLeft: String, private val stack: List[Any] = List()) {
    val json: String = jsonLeft.replaceAll("^\\s+", "")

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

    def value: T =
      stack.head.asInstanceOf[T]

    def map[U](f: T => U): State[U] =
      State(json, f(stack.head.asInstanceOf[T]) :: stack.tail)
  }
}

