/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package transform

/** A sample transform.
 */
abstract class Yolo extends Transform {
  // inherits abstract value `global` and class `Phase` from Transform

  import global._       // the global environment
  import typer.typed    // method to type trees

  /** the following two members override abstract members in Transform */
  val phaseName: String = "yolo"

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new SampleTransformer(unit)

  class SampleTransformer(unit: CompilationUnit) extends Transformer {

    override def transform(tree: Tree): Tree = {
      val tree1 = super.transform(tree);      // transformers always maintain `currentOwner`.
      tree1 match {
        case Apply(s @ Select(_, TermName("println")), List(Literal(Constant(string: String)))) => 
          typed(Apply(s, List(Literal(Constant(string + " #yolo")))))
        case _ =>
          tree1
      }
    }
  }
}
