package simplifier

import AST.{BinExpr, Node}

// Left association: BinExpr(op, BinExpr(), right)
class TripleOp(op: String) {
  def apply(first: Node, second: Node, third: Node): BinExpr =
      BinExpr(op, BinExpr(op, first, second), third)

  def unapply(elem: BinExpr): Option[(Node, Node, Node)] = elem match {
    case BinExpr(op1, BinExpr(op2, first, second), third)
      if op1 == op && op2 == op => Some((first, second, third))
    case BinExpr(op1, first, BinExpr(op2, second, third))
      if op1 == op && op2 == op => Some((first, second, third))
    case _ => None
  }
}
