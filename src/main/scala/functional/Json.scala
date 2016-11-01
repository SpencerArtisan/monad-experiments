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
  type Parser = State[Any] => Option[State[Any]]

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

  private def value(state: State[Any]): Option[State[Any]] =
    obj(state) |
    arr(state) |
    stringValue(state) |
    booleanValue(state) |
    numberValue(state) |
    nullValue(state)

  private def numberValue: Parser = (state) =>
    doubleValue(state) | intValue(state)

  private def intValue: Parser =
    regExValue("""-?\d+""", _.toInt)

  private def doubleValue: Parser =
    regExValue("""-?\d+\.\d+""", _.toDouble)

  private def booleanValue(state: State[Any]): Option[State[Any]] =
    trueValue(state) | falseValue(state)

  private def trueValue: Parser =
    regExValue("true", _ => true)

  private def falseValue: Parser =
    regExValue("false", _ => false)

  private def nullValue: Parser =
    regExValue("null", _ => null)

  private def stringValue: Parser =
    regExValue("\"(.+?)\"", identity)

  private def arr(state: State[Any]): Option[State[List[Any]]] =
    emptyArray(state) | nonEmptyArray(state)

  private def emptyArray(state: State[Any]): Option[State[List[Any]]] =
    regExValue("\\[\\]", _ => List())(state)

  private def nonEmptyArray(state: State[Any]): Option[State[List[Any]]] =
    Some(state) flatMap symbol("[") flatMap elements flatMap symbol("]") map asList

  private def elements(state: State[Any]): Option[State[Any]] =
    valueCommaThenElements(state) | value(state)

  private def valueCommaThenElements(state: State[Any]): Option[State[Any]] =
    Some(state) flatMap value flatMap ignoredSymbol(",") flatMap elements

  private def obj(state: State[Any]): Option[State[Map[String, Any]]] =
    emptyObj(state) | nonEmptyObj(state)

  private def emptyObj(state: State[Any]): Option[State[Map[String, Any]]] =
    regExValue("\\{\\}", _ => Map[String, Any]())(state)

  private def nonEmptyObj(state: State[Any]): Option[State[Map[String, Any]]] =
    state flatMap symbol("{") flatMap members flatMap symbol("}") map asMap

  private def members(state: State[Any]): Option[State[Any]] =
    tupleCommaThenMembers(state) | tuple(state)

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
    state.popTo("{").map(a => a.asInstanceOf[List[(String, Any)]].toMap)

  private def asList[A](state: State[Any]): State[List[Any]] =
    state.popTo("[")


  private def regExValue[B](pattern: String, result: String => B)(state: State[Any]): Option[State[B]] =
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

