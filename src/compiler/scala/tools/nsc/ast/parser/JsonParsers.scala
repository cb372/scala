package scala.tools.nsc
package ast.parser

import scala.reflect.internal.Chars.{ isScalaLetter }
import scala.reflect.internal.util.{ SourceFile, Position, FreshNameCreator, ListOfNil }
import Tokens._

trait JsonParsers {
  self: Parsers =>

  import global._

  class JsonParser(parser: SourceFileParser) {

    def jLiteral: Tree = {
      parser.in.nextToken()
      Literal(Constant("Some json"))
    }
    
  }

}
