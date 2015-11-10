package scala.tools.nsc
package ast.parser

import scala.json.AST._
import scala.collection.mutable.Stack
import scala.util.control.ControlThrowable
import Tokens._

trait JsonParsers {
  self: Parsers =>

  case object InvalidJsonControl extends ControlThrowable {
    override def getMessage = "Invalid json"
  }

  import global._

  /**
   * THE WORLD'S WORST JSON PARSER.
   * This implementation can be slow or buggy: pick two.
   */
  class JsonParser(parser: SourceFileParser) {
    private val stack = new Stack[Any]

    def jLiteral: Tree = {
      try {
        val jvalue = parse()
        //println(jvalue)
        parser.in.nextToken() // move to the next token after the json literal
        TreeBuilder.toTree(jvalue)
      } catch {
        case _: Throwable => parser.errorTermTree
      }
    }

    object TreeBuilder {
      private val _json: TermName = "json"
      private val _math: TermName = "math"

      private def _scala(name: TermName)            = Select(Select(Ident(nme.ROOTPKG), nme.scala_), name)
      private def _scala_math(name: TermName)       = Select(_scala(_math), name)
      private def _scala_json_ast(name: TermName)   = Select(Select(_scala(_json), "AST": TermName), name)
      private val _list                             = _scala("List": TermName)
      private val _bigdecimal                       = _scala_math("BigDecimal": TermName)
      private val _bigint                           = _scala_math("BigInt": TermName)
      private val _tuple2                           = _scala("Tuple2": TermName)

      def toTree(jvalue: JValue): Tree = jvalue match {
        case JNull => 
          _scala_json_ast("JNull")
        case JBool(value) => 
          if (value)
            Select(_scala_json_ast("JBool"), "True")
          else
            Select(_scala_json_ast("JBool"), "False")
        case JString(value) => 
          if (value.startsWith("IDENTIFIER::")) {
            Ident(TermName(value.replaceFirst("IDENTIFIER::", "")))
          } else
            Apply(_scala_json_ast("JString"), List(Literal(Constant(value))))
        case JDecimal(value) => 
          Apply(_scala_json_ast("JDecimal"), List(Apply(_bigdecimal, List(Literal(Constant(value.toString))))))
        case JInt(value) => 
          Apply(_scala_json_ast("JInt"), List(Apply(_bigint, List(Literal(Constant(value.toString))))))
        case JArray(elems) =>
          val listOfElems = elems map toTree
          Apply(_scala_json_ast("JArray"), List(Apply(_list, listOfElems)))
        case JObject(fields) =>
          val listOfFields = fields map { case (k, v) => 
            Apply(_tuple2, List(Literal(Constant(k)), toTree(v)))
          }
          Apply(_scala_json_ast("JObject"), List(Apply(_list, listOfFields)))
      }
    }

    private def oops(message: String) = {
      parser.syntaxError(parser.in.offset, message)
      throw InvalidJsonControl
    }

    private def parse(): JValue = {
      //println(s"Parsing! Current token = ${parser.in.token} (${parser.in.strVal}) and stack = $stack")
      parser.in.nextToken()
      //println(s"Next token = ${parser.in.token} (${parser.in.strVal})")
      parser.in.token match {
        case LBRACE =>
          stack.push(JObject(Nil))
          parse()
        case RBRACE =>
          scala.util.Try(stack.pop()).toOption match {
            case Some(obj: JObject) =>
              stack.headOption match {
                case Some((key: String, null)) =>
                  addFieldToParentObject(key, obj)
                  parse()
                case Some(arr: JArray) =>
                  addElemToParentArray(obj)
                  parse()
                case None =>
                  // finished
                  obj
                case other =>
                  oops(s"Unexpected object on stack: $other")
              }
            case _ =>
              oops("Unexpected right brace")
          }
        case LBRACKET =>
          stack.push(JArray(Nil))
          parse()
        case RBRACKET =>
          scala.util.Try(stack.pop()).toOption match {
            case Some(arr: JArray) =>
              stack.headOption match {
                case Some((key: String, null)) =>
                  addFieldToParentObject(key, arr)
                  parse()
                case Some(arr: JArray) =>
                  addElemToParentArray(arr)
                  parse()
                case None =>
                  // finished
                  arr
                case other =>
                  oops(s"Unexpected object on stack: $other")
              }
            case _ =>
              oops("Unexpected right bracket")
          }
        case STRINGLIT =>
          stack.headOption match {
            case Some(obj: JObject) =>
              // field name
              stack.push((parser.in.strVal, null))
              parse()
            case _ =>
              handleValue(JString(parser.in.strVal), "string")
          }
        case INTLIT =>
          handleValue(JInt(BigInt(parser.in.strVal)), "int")
        case DOUBLELIT =>
          handleValue(JDecimal(BigDecimal(parser.in.strVal)), "decimal")
        case TRUE =>
          handleValue(JBool.True, "true")
        case FALSE =>
          handleValue(JBool.False, "true")
        case NULL =>
          handleValue(JNull, "null")
        case IDENTIFIER | BACKQUOTED_IDENT =>
          handleValue(JString(s"IDENTIFIER::${parser.in.name.encode.toString}"), "identifier")
        case COMMA | NEWLINE =>
          parse()
        case COLON =>
          stack.headOption match {
            case Some((key: String, null)) =>
              parse()
            case _ =>
              oops("Unexpected colon")
          }
        case other =>
          oops(s"Unexpected token: $other")
      }
    }

    private def handleValue(jvalue: JValue, desc: String): JValue = stack.headOption match {
      case Some((key: String, null)) =>
        addFieldToParentObject(key, jvalue)
        parse()
      case Some(arr: JArray) =>
        addElemToParentArray(jvalue)
        parse()
      case None =>
        // finished
        jvalue
      case _ =>
        oops(s"Unexpected $desc")
    }

    private def addFieldToParentObject(key: String, value: JValue): Unit = {
      stack.pop() // remove the field from the stack
      stack.headOption match {
        case Some(JObject(xs)) =>
          stack.pop() // remove the parent object in order to replace it
          stack.push(JObject(xs :+ (key -> value))) // appendin' to a List like we just don't care
        case _ =>
          oops("Found a field that's not in an object")
      }
    }
    
    private def addElemToParentArray(value: JValue): Unit = stack.headOption match {
      case Some(JArray(xs)) =>
        stack.pop()
        stack.push(JArray(xs :+ value)) // appendin' to a List like we just don't care
      case _ =>
        oops("Expected to find an array at the head of the stack")
    }

  }

}
