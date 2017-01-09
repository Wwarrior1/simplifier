package simplifier

/**
  * Wojciech Baczyński i Piotr Grabiec
  */

import AST._

import scala.collection.mutable

object Simplifier {
  val Add = new BinOp("+")
  val Sub = new BinOp("-")
  val Mul = new BinOp("*")
  val Div = new BinOp("/")
  val Pow = new BinOp("**")
  val Add_Triple = new TripleOp("+")
  val Mul_Triple = new TripleOp("*")

  def simplify(node: Node): Node = node match {
    // "remove dead assignments"
    case NodeList(List(Assignment(x1, _), Assignment(x2, z))) if x1 == x2 => Assignment(x2, z)

    case NodeList(nodes) => NodeList(nodes map simplify filter checkBoundaryCondition) match {
      case NodeList(List(elem: NodeList)) => elem
      case _other => _other
    }

    case Tuple(nodes) => Tuple(nodes map simplify)

    case BinExpr(op, left, right) =>
      // Ensure expressions can be commutative only with commutative operators.
      if (Set("+", "*", "or", "and").contains(op))
        simplifyBinExpr(ensureCommutativity(BinExpr(op, simplify(left), simplify(right))))
      else
        simplifyBinExpr(BinExpr(op, simplify(left), simplify(right)))

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

    // --- Rest of simplifying from AST ---
    // (not necessary to pass actual tests)
    case Subscription(expr, sub) => Subscription(simplify(expr), simplify(sub))
    case KeyDatum(key, value) => KeyDatum(simplify(key), simplify(value))
    case GetAttr(expr, attr) => GetAttr(simplify(expr), attr)
    case IfInstr(cond, left) => IfInstr(simplify(cond), simplify(left))
    case WhileInstr(cond, body) => WhileInstr(simplify(cond), simplify(body))
    case ReturnInstr(e) => ReturnInstr(simplify(e))
    case PrintInstr(e) => PrintInstr(simplify(e))
    case FunCall(name, args_list) => FunCall(simplify(name), simplify(args_list))
    case FunDef(name, formal_args, body) => FunDef(name, simplify(formal_args), simplify(body))
    case LambdaDef(formal_args, body) => LambdaDef(simplify(formal_args), simplify(body))
    case ClassDef(name, inherit_list, suite) => ClassDef(name, simplify(inherit_list), simplify(suite))
    case ElemList(nodes) => ElemList(nodes map simplify)

    case _ => node
  }

  def checkBoundaryCondition(node: Node): Boolean = node match {
    case NodeList(List()) => false            // empty list
    case Assignment(x, y) if x == y => false  // "remove no effect instructions'
    case WhileInstr(FalseConst(), _) => false // "remove while loop with False condition"
    case _ => true
  }

  def simplifyBinExpr(node: BinExpr): Node = node match {
    // --- Recognize tuples & concatenate lists
    case BinExpr("+", Tuple(x), Tuple(y)) => Tuple(x ++ y)
    case BinExpr("+", ElemList(x), ElemList(y)) => ElemList(x ++ y)

    // --- Simplify expressions
    case Add(x, IntNum(0)) => x             // x+0 = x
    case Add(x, FloatNum(0)) => x           // x+0 = x
    case Sub(x, y) if x == y => IntNum(0)   // x-x = 0
    case Mul(x, IntNum(1)) => x             // x*1 = x
    case Mul(x, FloatNum(1)) => x           // x*1 = x
    case Mul(_, IntNum(0)) => IntNum(0)     // x*0 = 0
    case Mul(_, FloatNum(0)) => FloatNum(0)

    case BinExpr("or", x, y) if x == y => x               // x or x = x
    case BinExpr("and", x, y) if x == y => x              // x and x = x
    case BinExpr("or", _, TrueConst()) => TrueConst()     // x or True = True
    case BinExpr("and", _, FalseConst()) => FalseConst()  // x and False = False
    case BinExpr("or", x, FalseConst()) => x              // x or False = x
    case BinExpr("and", x, TrueConst()) => x              // x and True = x

    case BinExpr("==" | ">=" | "<=", x, y) if x == y => TrueConst()
    case BinExpr("!=" | ">" | "<", x, y) if x == y => FalseConst()

    // --- Unary: x+(-x) = 0
    case Add(Unary("-", x), y) if x == y => IntNum(0)
    case Add(x, Unary("-", y)) if x == y => IntNum(0)

    // --- Evaluating constants (and pow)
    case BinExpr(operator, IntNum(x), IntNum(y)) => IntNum(mapOperator(operator, x, y).toInt)
    case BinExpr(operator, IntNum(x), FloatNum(y)) => FloatNum(mapOperator(operator, x, y))
    case BinExpr(operator, FloatNum(x), IntNum(y)) => FloatNum(mapOperator(operator, x, y))
    case BinExpr(operator, FloatNum(x), FloatNum(y)) => FloatNum(mapOperator(operator, x, y))

    // --- Power laws
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
    // --- Triple ---
    // x**2 + 2*x*y + y**2" = (x+y)**2
    case Add_Triple(Pow(x1, IntNum(2)), Mul_Triple(x2, y1, IntNum(2)), Pow(y2, IntNum(2)))
      if x1 == x2 && y1 == y2 => simplify(BinExpr("**", BinExpr("+", x1, y1), IntNum(2)))
    // (x+y)**2 - x**2 - 2*x*y = y**2
    case BinExpr("-", BinExpr("-", BinExpr("**", BinExpr("+", x1, y1), IntNum(2)), Pow(x2, IntNum(2))), Mul_Triple(x3, y2, IntNum(2)))
      if x1 == x2 && x2 == x3 && y1 == y2 => simplify(BinExpr("**", y1, IntNum(2)))

    // --- "Understand distributive property of multiplication"
    case BinExpr("-", BinExpr("*", x1, IntNum(2)), x2)            // 2*x-x = x
      if x1 == x2 => x2
    case BinExpr("+", BinExpr("*", x, z1), BinExpr("*", y, z2))   // x*z+y*z = (x+y)*z
      if z1 == z2 => simplify(BinExpr("*", BinExpr("+", x, y), z2))
    case BinExpr("+", BinExpr("*", x1, y), BinExpr("*", x2, z))   // x*y+x*z = x*(y+z)
      if x1 == x2 => simplify(BinExpr("*", x2, BinExpr("+", y, z)))
    // x*y+x*z+v*y+v*z => (y+z)*x+v*y+v*z = (x+v)*(y+z)
    case BinExpr("+", BinExpr("+", BinExpr("*", BinExpr("+", y1, z1), x), BinExpr("*", v1, y2)), BinExpr("*", v2, z2))
      if y1 == y2 && z1 == z2 && v1 == v2 => simplify(BinExpr("*", BinExpr("+", x, v1), BinExpr("+", y1, z1)))

    // --- "Understand commutativity"
    case Sub(Add(x1, y), x2) if x1 == x2 => y // (x+5)-x = 5
    case Sub(x1, Add(x2, y)) if x1 == x2 => y // (5+x)-x = 5

    // --- "Simplify division"
    case Div(x, y) if x == y => IntNum(1)     // x/x = 1
    case Div(y1, Div(y2, x))                  // 1/(1/x) = x
      if Set(y1, y2) subsetOf Set[Node](IntNum(1), FloatNum(1)) => x
    case Mul(Div(num, y), x)                  // x*(1/y) = x/y
      if Set[Node](IntNum(1), FloatNum(1)) contains num => Div(x, y)

    case _ => node
  }

  // Ensure proper operators to be commutative.
  // Correct order: (binary -> unary -> variable -> const)
  def ensureCommutativity(node: BinExpr): BinExpr = node match {
    // --- BinOp ---
    // Const <=> Variable
    //   e.g. x+0 = 0+x // "simplify expressions"
    case BinExpr(op, const: Const, variable: Variable) =>
      BinExpr(op, variable, const)

    // Const, Variable, Unary <=> Binary
    //   e.g. x*y+x*z+v*y+v*z => (y+z)*x+v*y+v*z = (x+v)*(y+z) // "understand distributive property of multiplication"
    case BinExpr(op, const_variable_unary@(_: Const | _: Variable | _: Unary), binary: BinExpr) =>
      BinExpr(op, binary, const_variable_unary)

    // Variable <=> Variable
    //   e.g. (a or b) and (b or a) = a or b // "understand commutativity"
    case BinExpr(op, Variable(a), Variable(b))
      if a > b => BinExpr(op, Variable(b), Variable(a))

    // --- TripleOp ---
    // Const <=> Variable
    //   e.g. x**2 + 2*x*y + y**2 = (x+y)**2
    case BinExpr(op1, BinExpr(op2, x, y: Const), num: Variable)
      if op1 == op2 => BinExpr(op1, ensureCommutativity(BinExpr(op1, x, num)), y)

    case _ => node
  }

  def simplifyUnary(node: Unary): Node = node match {
    // Simplify double unary
    case Unary("-", Unary("-", x)) => x
    case Unary("not", Unary("not", x)) => x

    // Simplify 'not'
    case Unary("not", BinExpr("==", x, y)) => BinExpr("!=", x, y)
    case Unary("not", BinExpr("!=", x, y)) => BinExpr("==", x, y)
    case Unary("not", BinExpr(">", x, y)) => BinExpr("<=", x, y)
    case Unary("not", BinExpr("<", x, y)) => BinExpr(">=", x, y)
    case Unary("not", BinExpr(">=", x, y)) => BinExpr("<", x, y)
    case Unary("not", BinExpr("<=", x, y)) => BinExpr(">", x, y)

    // Evaluate constants
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
