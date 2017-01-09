package AST

object Priority {
  val binary = Map("lambda" -> 1,
    "or" -> 2,
    "and" -> 3,
    "is" -> 8, "<" -> 8, ">" -> 8, ">=" -> 8, "<=" -> 8, "==" -> 8, "!=" -> 8,
    "+" -> 9, "-" -> 9,
    "*" -> 10, "/" -> 10, "%" -> 10,
    "**" -> 11)

  val unary = Map("not" -> 4,
    "+" -> 12, "-" -> 12)
}

// sealed trait Node would be also OK
sealed abstract class Node {
  val indent: String = " " * 4

  def toStr = "error: toStr not implemented"
}

sealed abstract class Const extends Node

sealed abstract class Number extends Const

case class IntNum(value: Int) extends Number {
  override def toStr: String = value.toString
}

case class FloatNum(value: Double) extends Number {
  override def toStr: String = value.toString
}

case class StringConst(value: String) extends Node {
  override def toStr: String = value
}

case class TrueConst() extends Node {
  override def toStr = "True"
}

case class FalseConst() extends Node {
  override def toStr = "False"
}

case class Variable(name: String) extends Node {
  override def toStr: String = name
}

case class Unary(op: String, expr: Node) extends Node {
  override def toStr: String = {
    var str = expr.toStr
    expr match {
      case e@BinExpr(_, _, _) => if (Priority.binary(e.op) <= Priority.unary(op)) {
        str = "(" + str + ")"
      }
      case e@Unary(_, _) => if (Priority.unary(e.op) <= Priority.unary(op)) {
        str = "(" + str + ")"
      }
      case _ =>
    }
    op + " " + str
  }
}

case class BinExpr(op: String, left: Node, right: Node) extends Node {
  override def toStr: String = {
    var leftStr = left.toStr
    var rightStr = right.toStr
    left match {
      case l@(_: BinExpr) => if (Priority.binary(l.op) < Priority.binary(op)) {
        leftStr = "(" + leftStr + ")"
      }
      case l@(_: Unary) => if (Priority.unary(l.op) < Priority.binary(op)) {
        leftStr = "(" + leftStr + ")"
      }
      case _ =>
    }
    right match {
      case r@BinExpr(_, _, _) => if (Priority.binary(r.op) < Priority.binary(op)) {
        rightStr = "(" + rightStr + ")"
      }
      case r@Unary(_, _) => if (Priority.unary(r.op) < Priority.binary(op)) {
        rightStr = "(" + rightStr + ")"
      }
      case _ =>
    }
    leftStr + " " + op + " " + rightStr
  }
}

case class IfElseExpr(cond: Node, left: Node, right: Node) extends Node {
  override def toStr: String = left.toStr + " if " + cond.toStr + " else " + right.toStr
}

case class Assignment(left: Node, right: Node) extends Node {
  override def toStr: String = left.toStr + " = " + right.toStr
}

case class Subscription(expr: Node, sub: Node) extends Node {
  override def toStr: String = expr.toStr + "[" + sub.toStr + "]"
}

case class KeyDatum(key: Node, value: Node) extends Node {
  override def toStr: String = key.toStr + ": " + value.toStr
}

case class GetAttr(expr: Node, attr: String) extends Node {
  override def toStr: String = expr.toStr + "." + attr
}

case class IfInstr(cond: Node, left: Node) extends Node {
  override def toStr: String = {
    var str = "if " + cond.toStr + ":\n"
    str += left.toStr.replaceAll("(?m)^", indent)
    str
  }
}

case class IfElseInstr(cond: Node, left: Node, right: Node) extends Node {
  override def toStr: String = {
    var str = "if " + cond.toStr + ":\n"
    str += left.toStr.replaceAll("(?m)^", indent)
    str += "\nelse:\n"
    str += right.toStr.replaceAll("(?m)^", indent)
    str
  }
}

case class WhileInstr(cond: Node, body: Node) extends Node {
  override def toStr: String = {
    "while " + cond.toStr + ":\n" + body.toStr.replaceAll("(?m)^", indent)
  }
}

case class InputInstr() extends Node {
  override def toStr = "input()"
}

case class ReturnInstr(expr: Node) extends Node {
  override def toStr: String = "return " + expr.toStr
}

case class PrintInstr(expr: Node) extends Node {
  override def toStr: String = "print " + expr.toStr
}

case class FunCall(name: Node, args_list: Node) extends Node {
  override def toStr: String = {
    args_list match {
      case NodeList(list) => name.toStr + "(" + list.map(_.toStr).mkString("", ",", "") + ")"
      case _ => name.toStr + "(" + args_list.toStr + ")"
    }
  }
}

case class FunDef(name: String, formal_args: Node, body: Node) extends Node {
  override def toStr: String = {
    var str = "\ndef " + name + "(" + formal_args.toStr + "):\n"
    str += body.toStr.replaceAll("(?m)^", indent) + "\n"
    str
  }
}

case class LambdaDef(formal_args: Node, body: Node) extends Node {
  override def toStr: String = "lambda " + formal_args.toStr + ": " + body.toStr
}

case class ClassDef(name: String, inherit_list: Node, suite: Node) extends Node {
  override def toStr: String = {
    val str = "\nclass " + name
    var inheritStr = ""
    val suiteStr = ":\n" + suite.toStr.replaceAll("(?m)^", indent)
    inherit_list match {
      case NodeList(x) => if (x.nonEmpty) inheritStr = "(" + x.map(_.toStr).mkString("", ",", "") + ")"
      case _ =>
    }
    str + inheritStr + suiteStr
  }
}

case class NodeList(list: List[Node]) extends Node {
  override def toStr: String = {
    list.map(_.toStr).mkString("", "\n", "")
  }
}

case class KeyDatumList(list: List[KeyDatum]) extends Node {
  override def toStr: String = list.map(_.toStr).mkString("{", ",", "}")
}

case class IdList(list: List[Variable]) extends Node {
  override def toStr: String = list.map(_.toStr).mkString("", ",", "")
}

case class ElemList(list: List[Node]) extends Node {
  override def toStr: String = list.map(_.toStr).mkString("[", ",", "]")
}

case object EmptyNode extends Node {
  override def toStr = "EmptyNode"
}

case class Tuple(list: List[Node]) extends Node {
  override def toStr: String = if (list.isEmpty) "()"
  else if (list.length == 1) "(" + list.head.toStr + ",)"
  else list.map(_.toStr).mkString("(", ",", ")")
}
