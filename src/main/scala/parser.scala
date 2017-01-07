
import scala.util.parsing.combinator._
import scala.util.parsing.input.Positional
import AST._

class Parser extends JavaTokenParsers {

  protected override val whiteSpace = """(\t|\r|[ ]|#.*|(?s)/\*.*?\*/)+""".r
  val precedenceList: List[List[String]] = List(
    List("is", ">=", "<=", "==", "!=", "<", ">"), // order matters also within inner list, longer op should go before shorter one, e.g. "<=" before "<", if one is a prefix of another
    List("+", "-"),
    List("*", "/", "%")
  )
  val minPrec = 0
  val maxPrec = precedenceList.length - 1
  val id: Parser[String] = not(reserved) ~> """[a-zA-Z_]\w*""".r

  val lpar = rep1(newl) ~ "{" ~ rep(newl)
  val rpar = rep(newl) ~ "}" ~ rep1(newl)

  val newl: Parser[Node] = """(\r?\n)|\r""".r ^^ StringConst

  val reserved: Parser[String] = "and\\b".r |
    "class\\b".r |
    "def\\b".r |
    "else\\b".r |
    "False\\b".r |
    "if\\b".r |
    "is\\b".r |
    "input\\b".r |
    "lambda\\b".r |
    "not\\b".r |
    "or\\b".r |
    "print\\b".r |
    "return\\b".r |
    "True\\b".r |
    "while\\b".r

  val floatLiteral: Parser[Double] =
    """\d+(\.\d*)|\.\d+""".r ^^ {
      _.toDouble
    }

  val intLiteral: Parser[Integer] =
    """\d+""".r ^^ {
      _.toInt
    }

  def const: Parser[Node] = (
    floatLiteral ^^ FloatNum
      | intLiteral ^^ IntNum
      | stringLiteral ^^ StringConst
      | "True" ^^^ TrueConst()
      | "False" ^^^ FalseConst()
    )

  def parseAll(input: java.io.FileReader): ParseResult[List[Node]] = parseAll(program, input)

  // stands for def program: Parser[List[Node]] = rep(statement|newl)
  def program: Parser[List[Node]] = rep(newl) ~> rep(statement <~ rep(newl))


  def statement: Parser[Node] = (
    simple_statement
      | compound_statement
    )


  def expression: Parser[Node] = (
    ("lambda" ~> id_list <~ ":") ~ expression ^^ {
      case formal_args ~ body => LambdaDef(IdList(formal_args), body)
    }
      | or_expr
    )


  def or_expr: Parser[Node] = rep1sep(and_expr, "or") ^^ {
    case xs => (xs.head /: xs.tail) (BinExpr("or", _, _))
  }

  def and_expr: Parser[Node] = rep1sep(not_expr, "and") ^^ {
    case xs => (xs.head /: xs.tail) (BinExpr("and", _, _))
  }


  def not_expr: Parser[Node] = (

    "not" ~> not_expr ^^ (Unary("not", _)) // equivalent to "not"~>not_expr ^^ { case not_expr => Unary("not", not_expr) }
      | binary(minPrec) ~ opt(("if" ~> binary(minPrec) <~ "else") ~ expression) ^^ {
      case left ~ None => left
      case left ~ Some(cond ~ right) => IfElseExpr(cond, left, right)
    }
    )

  def target: Parser[Node] = (
    id ^^ Variable
      | subscription
      | get_attr
    )

  def subscription: Parser[Node] = (expression <~ "[") ~ expression <~ "]" ^^ {
    case expr ~ sub => Subscription(expr, sub)
  }

  def get_attr: Parser[Node] = (expression <~ ".") ~ id ^^ {
    case expression ~ id => GetAttr(expression, id)
  }


  def binary(level: Int): Parser[Node] = (
    if (level > maxPrec) unary
    else chainl1(binary(level + 1), binaryOp(level)) // equivalent to binary(level+1) * binaryOp(level)
    )

  // operator precedence parsing takes place here
  def binaryOp(level: Int): Parser[((Node, Node) => BinExpr)] = {
    precedenceList(level).map {
      op =>
        op ^^^ {
          ((a: Node, b: Node) => BinExpr(op, a, b))
        }
    }.reduce((head, tail) => head | tail)
  }


  def unary: Parser[Node] = (
    ("+" | "-") ~ unary ^^ { case "+" ~ expression => expression
    case "-" ~ expression => Unary("-", expression)
    }
      | primary
    )


  def primary: Parser[Node] = (
    lvalue
      | const
      | "(" ~> expression <~ ")"
      | "[" ~> expr_list_comma <~ "]" ^^ {
      case NodeList(x) => ElemList(x)
      case l => {
        println("Warn: expr_list_comma didn't return NodeList");
        l
      }
    }
      | "{" ~> key_datum_list <~ "}"
    )


  def lvalue: Parser[Node] = id ~ trailer ^^ {
    case id ~ list => foldTrailer(Variable(id), list)
  }

  def trailer: Parser[List[Tuple2[String, Node]]] = rep(
    "(" ~> expr_list <~ ")" ^^ { case expr_list => Tuple2("(", expr_list) }
      | "[" ~> expression <~ "]" ^^ { case expression => Tuple2("[", expression) }
      | "." ~> id ^^ { case id => Tuple2(".", Variable(id)) }
  )

  def foldTrailer(head: Node, list: List[Tuple2[String, Node]]): Node = {
    list match {
      case Tuple2(".", Variable(id)) :: tail => foldTrailer(GetAttr(head, id), tail)
      case Tuple2("(", attr) :: tail => foldTrailer(FunCall(head, attr), tail)
      case Tuple2("[", attr) :: tail => foldTrailer(Subscription(head, attr), tail)
      case _ => head
    }
  }

  def expr_list_comma: Parser[Node] = expr_list <~ opt(",")

  //def expr_list: Parser[Node] = repsep(expression, ",") ^^ NodeList // repsep returns List[Node]
  def expr_list: Parser[NodeList] = repsep(expression, ",") ^^ NodeList // repsep returns List[Node]
  // equivalent to (NodeList(_))
  // equivalent to { case expr_list => NodeList(expr_list) }

  //def key_datum_list: Parser[Node] = repsep(key_datum, ",") ^^ KeyDatumList
  def key_datum_list: Parser[KeyDatumList] = repsep(key_datum, ",") ^^ KeyDatumList

  // equivalent to (KeyDatumList(_))
  // equivalent to { case key_datum_list => KeyDatumList(key_datum_list) }

  def key_datum: Parser[KeyDatum] = expression ~ ":" ~ expression ^^ {
    case key ~ ":" ~ value => KeyDatum(key, value)
  }

  def funcdef: Parser[FunDef] = ("def" ~> id <~ "(") ~ id_list ~ ((")" ~ ":") ~> suite) ^^ {
    case id ~ id_list ~ suite => FunDef(id, IdList(id_list), suite)
  }

  def classdef: Parser[ClassDef] = ("class" ~> id) ~ ("(" ~> expr_list <~ ")").? ~ (":" ~> suite) ^^ {
    case id ~ Some(expr_list) ~ suite => ClassDef(id, expr_list, suite)
    case id ~ None ~ suite => ClassDef(id, NodeList(List()), suite)
  }


  def id_list: Parser[List[Variable]] = repsep(id, ",") ^^ {
    case id_list => id_list.map(Variable)
  }

  def suite: Parser[Node] = lpar ~> statement_list <~ rpar


  def statement_list: Parser[Node] = rep1(statement) ^^ NodeList

  // equivalent to (NodeList(_))
  // equivalent to { case statement_list => NodeList(statement_list) }

  def simple_statement: Parser[Node] = (small_statement_list <~ ';'.?) <~ rep1(newl) ^^ NodeList

  // equivalent to (NodeList(_))
  // equivalent to { case small_statement_list => NodeList(small_statement_list) }

  def small_statement: Parser[Node] = (
    print_instr
      | return_instr
      | assignment
      | expression
    )

  def small_statement_list: Parser[List[Node]] = rep1sep(small_statement, ";")

  def compound_statement: Parser[Node] = (
    if_else_stmt
      | while_stmt
      | funcdef
      | classdef
    )


  def print_instr: Parser[PrintInstr] = "print" ~> expression ^^ PrintInstr

  def return_instr: Parser[ReturnInstr] = "return" ~> expression ^^ ReturnInstr

  def assignment: Parser[Assignment] = (target <~ "=") ~ expression ^^ {
    case target ~ expression => Assignment(target, expression)
  }

  def if_else_stmt: Parser[Node] = (
    "if" ~> expression ~ (":" ~> suite) ~ ("else" ~ ":" ~> suite).? ^^ {
      case expression ~ suite1 ~ Some(suite2) => IfElseInstr(expression, suite1, suite2)
      case expression ~ suite ~ None => IfInstr(expression, suite)
    }
    )

  def while_stmt: Parser[WhileInstr] = "while" ~> expression ~ (":" ~> suite) ^^ {
    case expression ~ suite => WhileInstr(expression, suite)
  }

  def input_instr: Parser[InputInstr] = "input" ~ "(" ~ ")" ^^^ InputInstr()

}

