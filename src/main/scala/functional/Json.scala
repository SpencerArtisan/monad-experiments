package functional

import java.util.regex.Pattern

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
    def | [B >: A](alternative: => Option[B]): Option[B] = o.orElse(alternative)
    def andThen [B](f: A => Option[B]): Option[B] = o.flatMap[B](f)
    def as [B](f: A => B): Option[B] = o.map(f)
  }

  def parse(jsonText: String): Json =
    Json(Option(jsonText) map {State(_)} flatMap value map {_.data})

  private def value: Parser[Any] = (state) =>
    obj(state) |
    arr(state) |
    stringValue(state) |
    booleanValue(state) |
    numberValue(state) |
    nullValue(state)

  private def numberValue: Parser[AnyVal] = (state) =>
    doubleValue(state) | intValue(state)

  private def intValue: Parser[Int] =
    regExValue("""-?\d+""", _.toInt)

  private def doubleValue: Parser[Double] =
    regExValue("""-?\d+\.\d+""", _.toDouble)

  private def booleanValue: Parser[Boolean] = (state) =>
    trueValue(state) | falseValue(state)

  private def trueValue: Parser[Boolean] =
    constValue("true", _ => true)

  private def falseValue: Parser[Boolean] =
    constValue("false", _ => false)

  private def nullValue: Parser[Null] =
    constValue("null", _ => null)

  private def stringValue: Parser[String] =
    regExValue("\"(.+?)\"", identity)

  private def constValue[B](value: String, result: String => B): Parser[B] =
    regExValue(Pattern.quote(value), result)

  private def arr: Parser[List[Any]] = (state) =>
    emptyArray(state) | nonEmptyArray(state)

  private def emptyArray: Parser[List[Any]] =
    constValue("[]", _ => List())

  private def nonEmptyArray: Parser[List[Any]] = (state) =>
    state >> symbol("[") andThen elements andThen symbol("]") as list

  private def elements: Parser[Any] = (state) =>
    (state >> value andThen ignore(",") andThen elements) | (state >> value)

  private def obj: Parser[Map[String, Any]] = (state) =>
    (state >> emptyObj) | (state >> nonEmptyObj)

  private def emptyObj: Parser[Map[String, Any]] =
    constValue("{}", _ => Map[String, Any]())

  private def nonEmptyObj: Parser[Map[String, Any]] = (state) =>
    state >> symbol("{") andThen members andThen symbol("}") as map

  private def members: Parser[Any] = (state) =>
    (state >> tuple andThen ignore(",") andThen members) | (state >> tuple)

  private def tuple: Parser[(String, Any)] = (state) =>
    state >> stringValue andThen ignore(":") andThen value as pair

  private def symbol(symbol: String): Parser[Any] = (state) =>
    state >> constValue(symbol, identity)

  private def ignore(symbol: String): Parser[Any] = (state) =>
    state >> constValue(symbol, _ => null)

  private def pair[A](state: State[Any]): State[(String, Any)] =
    state.popTuple()

  private def map[A](state: State[Any]): State[Map[String, Any]] =
    state.popTo("{").map(a => a.asInstanceOf[List[(String, Any)]].toMap)

  private def list[A](state: State[Any]): State[List[Any]] =
    state.popTo("[")

  private def regExValue[B](pattern: String, result: String => B): Parser[B] = (state) =>
    ("^" + pattern).r.findFirstMatchIn(state.json).map(m => state.advance(m.group(0).length, result(m.group(m.groupCount))))

  
  case class State[+T](private val jsonLeft: String, private val stack: List[Any] = List()) {
    val json: String = jsonLeft.replaceAll("^\\s+", "")

    def advance(chars: Int): State[T] =
      State(json.substring(chars), stack)

    def advance[U](chars: Int, newData: U): State[U] =
      State(json.substring(chars), (newData == null) ? stack | newData :: stack)

    def popTo(marker: Any): State[List[Any]] =
      State(json, stack.drop(1).takeWhile(item => item != marker).reverse :: stack.dropWhile(item => item != marker).drop(1))

    def popTuple(marker: Any): State[(String, Any)] =
      State(json, (stack.tail.head, stack.head) :: stack.drop(2))

    def map[U](f: T => U): State[U] =
      State(json, f(stack.head.asInstanceOf[T]) :: stack.tail)

    def data: T =
      stack.head.asInstanceOf[T]

    def >>[A](parser: Parser[A]):Option[State[A]] = Some(this) andThen parser
  }
}

