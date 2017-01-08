package simplifier

import AST._

// to implement
// avoid one huge match of cases
// take into account non-greedy strategies to resolve cases with power laws
object Simplifier {

  def simplify(node: Node): Node = node match {
    case NodeList(nodes) => NodeList(nodes map simplify filter hasEffect) match {
      case NodeList(List(elem: NodeList)) => elem
      case _other => _other
    }

    case BinExpr(op, left, right) =>
//      if (Set("+", "*", "or", "and").contains(op))
//        simplifyBinExpr(reorder(BinExpr(op, simplify(left), simplify(right))))
//      else
        simplifyBinExpr(BinExpr(op, simplify(left), simplify(right)))

    case Tuple(nodes) => Tuple(nodes map simplify)

    case _ => node
  }

  def hasEffect(node: Node): Boolean = node match { // TODO change name to check
    case NodeList(List()) => false  // empty list

    case _ => true  // default
  }

  def simplifyBinExpr(node: BinExpr): Node = node match {
    // Concatenating tuples and lists
    case BinExpr("+", Tuple(a), Tuple(b)) => Tuple(a ++ b)
    case BinExpr("+", ElemList(a), ElemList(b)) => ElemList(a ++ b)
    case _ => node
  }
}
