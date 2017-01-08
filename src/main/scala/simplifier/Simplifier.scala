package simplifier

import AST._

// to implement
// avoid one huge match of cases
// take into account non-greedy strategies to resolve cases with power laws
object Simplifier {
  val Add2 = new BinOp("+")
  val Sub2 = new BinOp("-")
  val Mul2 = new BinOp("*")
  val Div2 = new BinOp("/")

  def simplify(node: Node): Node = node match {
    case NodeList(nodes) => NodeList(nodes map simplify filter checkBoundaryCondition) match {
      case NodeList(List(elem: NodeList)) => elem
      case _other => _other
    }

    case BinExpr(op, left, right) =>
      //      if (Set("+", "*", "or", "and").contains(op))
      simplifyBinExpr(ensureProperOrder(BinExpr(op, simplify(left), simplify(right))))
    //      else
    //        simplifyBinExpr(BinExpr(op, simplify(left), simplify(right)))

    case Unary(op, expr) => simplifyUnary(Unary(op, simplify(expr)))

    case Tuple(nodes) =>
      Tuple(nodes map simplify)

    case _ => node
  }

  def checkBoundaryCondition(node: Node): Boolean = node match {
    case NodeList(List()) => false // empty list

    case _ => true // default
  }

  def simplifyBinExpr(node: BinExpr): Node = node match {
    // --- recognize tuples & concatenate lists
    case BinExpr("+", Tuple(a), Tuple(b)) => Tuple(a ++ b)
    case BinExpr("+", ElemList(a), ElemList(b)) => ElemList(a ++ b)

    // --- simplify expressions
    //x + 0 = x
    case Add2(x, IntNum(0)) => x
    case Add2(x, FloatNum(0)) => x

    //x - x = 0
    case Sub2(x, y) if x == y => IntNum(0)

    //x * 1 = x
    case Mul2(x, IntNum(1)) => x
    case Mul2(x, FloatNum(1)) => x

    //x * 0 = 0
    case Mul2(x, IntNum(0)) => IntNum(0)
    case Mul2(x, FloatNum(0)) => FloatNum(0)

    case BinExpr("or", x, y) if x == y => x               // x or x = x
    case BinExpr("and", x, y) if x == y => x              // x and x = x
    case BinExpr("or", x, TrueConst()) => TrueConst()     // x or True = True
    case BinExpr("and", x, FalseConst()) => FalseConst()  // x and False = False
    case BinExpr("or", x, FalseConst()) => x              // x or False = x
    case BinExpr("and", x, TrueConst()) => x              // x and True = x

    // x == x | x >= y | x <= y = True
    case BinExpr("==" | ">=" | "<=", x, y) if x == y => TrueConst()
    // x != x | x > y | x < y = False
    case BinExpr("!=" | ">" | "<", x, y) if x == y => FalseConst()

    case Div2(x, y) if x == y => IntNum(1)    // x / x = 1

    // Unary: x + (-x) = 0
    case Add2(Unary("-", x), y) if x == y => IntNum(0)
    case Add2(x, Unary("-", y)) if x == y => IntNum(0)

    case _ => node
  }

  //reordering commutative operators in order:
  //binary, unary, var, const
  def ensureProperOrder(node: BinExpr): BinExpr = node match {
    //a _ b = b _ a; const <=> var
    case BinExpr(op, a: Const, b: Variable) => BinExpr(op, b, a)

    case _ => node
  }

  def simplifyUnary(node: Unary): Node = node match {
    // cancel double unary ops
    case Unary("-", Unary("-", x)) => x
    case Unary("not", Unary("not", x)) => x

    // get rid of not before comparisons
    case Unary("not", BinExpr("==", x, y)) => BinExpr("!=", x, y)
    case Unary("not", BinExpr("!=", x, y)) => BinExpr("==", x, y)
    case Unary("not", BinExpr(">", x, y)) => BinExpr("<=", x, y)
    case Unary("not", BinExpr("<", x, y)) => BinExpr(">=", x, y)
    case Unary("not", BinExpr(">=", x, y)) => BinExpr("<", x, y)
    case Unary("not", BinExpr("<=", x, y)) => BinExpr(">", x, y)

    // evaluate constants
    case Unary("not", TrueConst()) => FalseConst()
    case Unary("not", FalseConst()) => TrueConst()

    case _ => node
  }

}
