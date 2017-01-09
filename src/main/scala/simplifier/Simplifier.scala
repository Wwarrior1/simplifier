package simplifier

import AST._

import scala.collection.mutable

// to implement
// avoid one huge match of cases
// take into account non-greedy strategies to resolve cases with power laws
object Simplifier {
  val Add2 = new BinOp("+")
  val Sub2 = new BinOp("-")
  val Mul2 = new BinOp("*")
  val Div2 = new BinOp("/")

  def simplify(node: Node): Node = node match {
    // "remove dead assignments"
    case NodeList(List(Assignment(x1, _), Assignment(x2, z))) if x1 == x2 => Assignment(x2, z)

    case NodeList(nodes) => NodeList(nodes map simplify filter checkBoundaryCondition) match {
      case NodeList(List(elem: NodeList)) => elem
      case _other => _other
    }

    case Tuple(nodes) => Tuple(nodes map simplify)

    case BinExpr(op, left, right) =>
      //      if (Set("+", "*", "or", "and").contains(op))
      simplifyBinExpr(ensureProperOrder(BinExpr(op, simplify(left), simplify(right))))
    //      else
    //        simplifyBinExpr(BinExpr(op, simplify(left), simplify(right)))

    case Unary(op, expr) => simplifyUnary(Unary(op, simplify(expr)))

    // "remove duplicate keys"
    case KeyDatumList(list) => KeyDatumList(simplifyDuplicatedKeys(list))

    case IfElseInstr(cond, left, right) => simplify(cond) match {
      case TrueConst() => simplify(left)
      case FalseConst() => simplify(right)
      case _cond => IfElseInstr(_cond, simplify(left), simplify(right))
    }

    case Assignment(left, right) => Assignment(simplify(left), simplify(right))
    case IfElseExpr(cond, left, right) => simplify(cond) match {
      case TrueConst() => simplify(left)
      case FalseConst() => simplify(right)
      case _cond => IfElseExpr(_cond, simplify(left), simplify(right))
    }

    case _ => node
  }

  def checkBoundaryCondition(node: Node): Boolean = node match {
    case NodeList(List()) => false // empty list
    case Assignment(x, y) if x == y => false // "remove no effect instructions'
    case WhileInstr(FalseConst(), _) => false // "remove while loop with False condition"

    case _ => true // default
  }

  def simplifyBinExpr(node: BinExpr): Node = node match {
    // --- recognize tuples & concatenate lists
    case BinExpr("+", Tuple(a), Tuple(b)) => Tuple(a ++ b)
    case BinExpr("+", ElemList(a), ElemList(b)) => ElemList(a ++ b)

    // --- simplify expressions
    case Add2(x, IntNum(0)) => x // x+0 = x
    case Add2(x, FloatNum(0)) => x // x+0 = x
    case Sub2(x, y) if x == y => IntNum(0) // x-x = 0
    case Mul2(x, IntNum(1)) => x // x*1 = x
    case Mul2(x, FloatNum(1)) => x // x*1 = x
    case Mul2(x, IntNum(0)) => IntNum(0) // x*0 = 0
    case Mul2(x, FloatNum(0)) => FloatNum(0)

    case BinExpr("or", x, y) if x == y => x // x or x = x
    case BinExpr("and", x, y) if x == y => x // x and x = x
    case BinExpr("or", x, TrueConst()) => TrueConst() // x or True = True
    case BinExpr("and", x, FalseConst()) => FalseConst() // x and False = False
    case BinExpr("or", x, FalseConst()) => x // x or False = x
    case BinExpr("and", x, TrueConst()) => x // x and True = x

    // x == x | x >= y | x <= y = True
    case BinExpr("==" | ">=" | "<=", x, y) if x == y => TrueConst()
    // x != x | x > y | x < y = False
    case BinExpr("!=" | ">" | "<", x, y) if x == y => FalseConst()

    case Div2(x, y) if x == y => IntNum(1) // x / x = 1

    // Unary: x + (-x) = 0
    case Add2(Unary("-", x), y) if x == y => IntNum(0)
    case Add2(x, Unary("-", y)) if x == y => IntNum(0)

    // Evaluating constants (and pow)
    case BinExpr(operator, IntNum(a), IntNum(b)) => IntNum(mapOperator(operator, a, b).toInt)
    case BinExpr(operator, IntNum(a), FloatNum(b)) => FloatNum(mapOperator(operator, a, b))
    case BinExpr(operator, FloatNum(a), IntNum(b)) => FloatNum(mapOperator(operator, a, b))
    case BinExpr(operator, FloatNum(a), FloatNum(b)) => FloatNum(mapOperator(operator, a, b))

    // Power laws
    case BinExpr("**", _, IntNum(0)) => IntNum(1) // x**0 = 1
    case BinExpr("**", _, FloatNum(0.0)) => FloatNum(1.0)
    case BinExpr("**", x, IntNum(1)) => x // x**1 = x
    case BinExpr("**", x, FloatNum(1)) => x
    // x**y * x**z = x ** (y+z)
    case BinExpr("*", BinExpr("**", x1, y), BinExpr("**", x2, z))
      if x1 == x2 => simplify(BinExpr("**", x1, BinExpr("+", y, z)))
    //(x**n)**m" = x**(n*m)
    case BinExpr("**", BinExpr("**", x, n), m) => simplify(BinExpr("**", x, BinExpr("*", n, m)))
    //(x+y)**2 - (x-y)**2 = 4*x*y
    case BinExpr("-", BinExpr("**", BinExpr("+", x1, y1), IntNum(2)), BinExpr("**", BinExpr("-", x2, y2), IntNum(2)))
      if x1 == x2 && y1 == y2 => simplify(BinExpr("*", BinExpr("*", IntNum(4), x1), y1))

    // "understand distributive property of multiplication"
    case BinExpr("-", BinExpr("*", x1, IntNum(2)), x2) // 2*x-x = x
      if x1 == x2 => x2
    case BinExpr("+", BinExpr("*", x, z1), BinExpr("*", y, z2)) // x*z+y*z = (x+y)*z
      if z1 == z2 => simplify(BinExpr("*", BinExpr("+", x, y), z2))
    case BinExpr("+", BinExpr("*", x1, y), BinExpr("*", x2, z)) // x*y+x*z = x*(y+z)
      if x1 == x2 => simplify(BinExpr("*", x2, BinExpr("+", y, z)))

    // understand commutativity
    case Sub2(Add2(x1, y), x2) if x1 == x2 => y // (x+5)-x = 5
    case Sub2(x1, Add2(x2, y)) if x1 == x2 => y // (5+x)-x = 5

    case _ => node
  }

  // Ensure operators are in order: binary -> unary -> var -> const
  def ensureProperOrder(node: BinExpr): BinExpr = node match {
    // TODO
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

  def simplifyDuplicatedKeys(list: List[KeyDatum]): List[KeyDatum] = {
    val returnMap: mutable.LinkedHashMap[Node, Node] = mutable.LinkedHashMap()
    for (element <- list)
      returnMap.update(simplify(element.key), simplify(element.value))
    returnMap.toList.map(KeyDatum.tupled)
  }

  def mapOperator(operator: String, x: Double, y: Double): Double = operator match {
    case "+" => x + y
    case "-" => x - y;
    case "*" => x * y
    case "/" => x / y;
    case "**" => math.pow(x, y);
  }
}
