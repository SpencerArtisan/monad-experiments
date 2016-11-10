A Monadic Functional JSON Parser.  
==
written in Scala

Usage
--

I'd recommend just copying the Json.scala file into your project.  It's only one file.

To parse the json

~~~~
val json = Json.parse(jsonString)
~~~~

Nodes are returned as Json objects, which can also be treated as scala options of the node values.

~~~~
val node = json > [node name]
val nodeValue = node.getOrElse([default value])
~~~~

To access a node in the hierarchy

~~~~
json > [node name] > [sub node name]
~~~~

To access an element in an array

~~~~
json > [array node name] > [node index]
~~~~


Notes
--

This implementation was written as a learning experiment with Scalaz. I don't know if it's a sensible way to implement a parser, but I wanted to see if I could create a monadic functional parser without any mutable state. I also wanted to avoid the standard Abstract Syntax Tree method of parsing, not for any better reason than to see if I could; I was aiming at code with a close proximity to the json syntax definition at http://www.json.org.

Syntax definition:

~~~~
object
    {}
    { members }
members
    pair
    pair , members
pair
    string : value
array
    []
    [ elements ]
elements
    value 
    value , elements
value
    string
    number
    object
    array
    true
    false
    null
~~~~

Code sample:

~~~~
private def value: Parser[Any] = (state) =>
    state >> obj |
    state >> arr |
    state >> stringValue |
    state >> booleanValue |
    state >> numberValue |
    state >> nullValue

private def arr: Parser[List[Any]] = (state) =>
    state >> emptyArray | state >> nonEmptyArray

private def emptyArray: Parser[List[Any]] =
    constValue("[]", _ => List())

private def nonEmptyArray: Parser[List[Any]] = (state) =>
    state >> symbol("[") andThen elements andThen symbol("]") as list

private def elements: Parser[Any] = (state) =>
    (state >> value andThen ignore(",") andThen elements) | state >> value
~~~~

Left To Do
--

- Performance testing
- Yaml implementation
- Upload to maven central
- Find a way to avoid having state as an explicit parameter. Applicative functors?


