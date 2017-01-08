package simplifier

import AST.{BinExpr, Node}

class BinOp(op: String) {
  def apply(left: Node, right: Node) =
    BinExpr(op, left, right)

  def unapply(elem: BinExpr): Option[(Node, Node)] =
    if (elem.op == op) Some((elem.left, elem.right))
    else None
}
