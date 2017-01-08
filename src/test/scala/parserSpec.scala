import AST._
import org.specs2.mutable._
import simplifier.Simplifier.simplify

class parserSpec extends Specification {
  val parser = new Parser()

  def parseString(str: String): Node = {
    val parseResult = parser.parseAll(parser.program, str + "\n")

    parseResult match {
      case parser.Success(result: List[AST.Node], in) => simplify(NodeList(result))
      case parser.NoSuccess(msg: String, in) => throw new IllegalArgumentException("FAILURE Could not parse '" + str + "': " + msg)
    }
  }

  "parser" should {

    "fail on incorrect input" in {
      parseString("x+y=x") must throwA[IllegalArgumentException]
      parseString("a not b") must throwA[IllegalArgumentException]
    }

    "recognize elif branches in if-else stmts" in {
      val if_stmt_str =
        """if x>1:
             {
                y = -1
             }
             elif x>0:
             {
                y = 1
             }
             elif x>-1:
             {
                y = -1
             }
             else:
             {
                y = 1
             } """

      parseString(if_stmt_str) must not(throwA[IllegalArgumentException])
      parseString(if_stmt_str) mustEqual parseString(
        """if x>1:
             {
                y = -1
             }
             else:
             {
          	    if x>0:
          	    {
          	      y = 1
          	    }
               else:
          	    {
          	      if x>-1:
          	       {
          		        y = -1
          	       }
          	       else:
          	       {
          		        y = 1
          	       }
          	    }
             }""")
    }

    "parse expressions" in {
      parser.parseAll(parser.expression, "True").get mustEqual TrueConst()
      parser.parseAll(parser.expression, "False").get mustEqual FalseConst()
      parser.parseAll(parser.expression, "1").get mustEqual IntNum(1)
      parser.parseAll(parser.expression, "a").get mustEqual Variable("a")
      parser.parseAll(parser.expression, "-a").get mustEqual Unary("-", Variable("a"))
      parser.parseAll(parser.expression, "a+b").get mustEqual BinExpr("+", Variable("a"), Variable("b"))
      parser.parseAll(parser.expression, "not a+b").get mustEqual Unary("not", BinExpr("+", Variable("a"), Variable("b")))
      parser.parseAll(parser.expression, "f(x)").get mustEqual FunCall(Variable("f"), NodeList(List(Variable("x"))))
      parser.parseAll(parser.expression, "x.y").get mustEqual GetAttr(Variable("x"), "y")
    }

  }

  //  "simplifier" should {
  //
  //    "recognize tuples" in {
  //      parseString("x=(a,b,c)") must not(throwA[IllegalArgumentException])
  //      parseString("(x,y)+(u,v)") mustEqual parseString("(x,y,u,v)")
  //    }
  //
  //    "recognize power laws" in {
  //      parseString("x**y*x**z") must not(throwA[IllegalArgumentException])
  //      parseString("x**y*x**z") mustEqual parseString("x**(y+z)")
  //      parseString("2**3**2") mustEqual parseString("512")
  //      parseString("x**0") mustEqual parseString("1")
  //      parseString("x**1") mustEqual parseString("x")
  //      parseString("(x**n)**m") mustEqual parseString("x**(n*m)")
  //      parseString("x**2+2*x*y+y**2") mustEqual parseString("(x+y)**2")
  //      parseString("(x+y)**2-x**2-2*x*y") mustEqual parseString("y**2")
  //      parseString("(x+y)**2-(x-y)**2") mustEqual parseString("4*x*y")
  //    }
  //
  //    "evaluate constants" in {
  //      parseString("2+3*5") mustEqual parseString("17")
  //      parseString("not False") mustEqual parseString("True")
  //      parseString("not True") mustEqual parseString("False")
  //    }
  //
  //    "simplify division" in {
  //      parseString("x/x") mustEqual parseString("1")
  //      parseString("(x+y*z)/(x+y*z)") mustEqual parseString("1")
  //      parseString("(x+y)/(y+x)") mustEqual parseString("1")
  //      parseString("(x+y*z)/(y*z+x)") mustEqual parseString("1")
  //      parseString("1/(1/x)") mustEqual parseString("x")
  //      parseString("1/(1/(x-z))") mustEqual parseString("x-z")
  //      parseString("x*(1/y)") mustEqual parseString("x/y")
  //    }
  //
  //    "simplify expressions" in {
  //      parseString("x+0") mustEqual parseString("x")
  //      parseString("0+x") mustEqual parseString("x")
  //      parseString("x-x") mustEqual parseString("0")
  //      parseString("-x+x") mustEqual parseString("0")
  //      parseString("x*1") mustEqual parseString("x")
  //      parseString("1*x") mustEqual parseString("x")
  //      parseString("0*x") mustEqual parseString("0")
  //      parseString("x or x") mustEqual parseString("x")
  //      parseString("x or x") mustEqual parseString("x")
  //      parseString("x and x") mustEqual parseString("x")
  //      parseString("x or True") mustEqual parseString("True")
  //      parseString("x or False") mustEqual parseString("x")
  //      parseString("x and False") mustEqual parseString("False")
  //      parseString("x and True") mustEqual parseString("x")
  //      parseString("x==x") mustEqual parseString("True")
  //      parseString("x>=x") mustEqual parseString("True")
  //      parseString("x<=x") mustEqual parseString("True")
  //      parseString("x!=x") mustEqual parseString("False")
  //      parseString("x>x") mustEqual parseString("False")
  //      parseString("x<x") mustEqual parseString("False")
  //    }
  //
  //    "understand commutativity" in {
  //      parseString("x+5-x") mustEqual parseString("5")
  //      parseString("(a or b) and (b or a)") mustEqual parseString("a or b")
  //      parseString("(a and b) or (b and a)") mustEqual parseString("a and b")
  //    }
  //
  //    "understand distributive property of multiplication" in {
  //      parseString("2*x-x") mustEqual parseString("x")
  //      parseString("x*z+y*z") mustEqual parseString("(x+y)*z")
  //      parseString("x*y+x*z") mustEqual parseString("x*(y+z)")
  //      parseString("x*y+x*z+v*y+v*z") mustEqual parseString("(x+v)*(y+z)")
  //    }
  //
  //    "cancel double unary ops" in {
  //      parseString("not not not x") mustEqual parseString("not x")
  //      parseString("--x") mustEqual parseString("x")
  //    }
  //
  //    "get rid of not before comparisons" in {
  //      parseString("not x==y") mustEqual parseString("x!=y")
  //      parseString("not x!=y") mustEqual parseString("x==y")
  //      parseString("not x>y") mustEqual parseString("x<=y")
  //      parseString("not x<y") mustEqual parseString("x>=y")
  //      parseString("not x>=y") mustEqual parseString("x<y")
  //      parseString("not x<=y") mustEqual parseString("x>y")
  //    }
  //
  //    "remove duplicate keys" in {
  //      parseString("""{ "a": 1, "b": 2, "a": 3 }""") mustEqual parseString("""{ "a": 3, "b": 2 }""")
  //    }
  //
  //    "concatenate lists" in {
  //      parseString("[]+[]") mustEqual parseString("[]")
  //      parseString("[a,b,c]+[]") mustEqual parseString("[a,b,c]")
  //      parseString("[]+[a,b,c]") mustEqual parseString("[a,b,c]")
  //      parseString("[a,b,c]+[x,y]") mustEqual parseString("[a,b,c,x,y]")
  //    }
  //
  //    "remove no effect instructions" in {
  //      parseString("x=x") mustEqual parseString("")
  //    }
  //
  //    "remove dead assignments" in {
  //      parseString("x=a; x=b") mustEqual parseString("x=b")
  //    }
  //
  //    "simplify if-else instruction with known condition" in {
  //      val if_stmt_str =
  //        """if %s:
  //                   {
  //                      x = 1
  //                   }
  //                   else:
  //                   {
  //                      x = 0
  //                   } """
  //
  //      parseString(if_stmt_str.format("True")) mustEqual parseString("x=1")
  //      parseString(if_stmt_str.format("False")) mustEqual parseString("x=0")
  //    }
  //
  //    "simplify if-else expression with known condition" in {
  //      val if_expr_str = "x = y if %s else z"
  //      parseString(if_expr_str.format("True")) mustEqual parseString("x=y")
  //      parseString(if_expr_str.format("False")) mustEqual parseString("x=z")
  //    }
  //
  //    "remove while loop with False condition" in {
  //      val str =
  //        """while False:
  //                   {
  //                      x = x + 1
  //                   } """
  //
  //      parseString(str) mustEqual parseString("")
  //    }
  //
  //  }

}
