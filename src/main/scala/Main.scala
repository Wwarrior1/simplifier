import java.io.{FileNotFoundException, FileReader, IOException}

object Main {
  def main(args: Array[String]) {

    if (args.length == 0) {
      println("Usage: sbt \"run filename ...\"")
      return
    }

    val parser = new Parser()

    for (arg <- args) {
      try {
        println("Parsing file: " + arg)
        val reader = new FileReader(arg)
        val parseResult = parser.parseAll(reader)
        parseResult match {
          case parser.Success(result: List[AST.Node], in) => {
            println("\nAST:")
            println(parseResult)
            val tree = AST.NodeList(result)
            val simplifiedTree = simplifier.Simplifier.simplify(tree)
            println("\nAST after optimization:")
            println(simplifiedTree)
            println("\nProgram after optimization:")
            println(simplifiedTree.toStr)
          }
          case parser.NoSuccess(msg: String, in) => println("FAILURE " + parseResult)
        }
      }
      catch {
        case ex: FileNotFoundException => println("Couldn't open file " + arg)
        case ex: IOException => println("Couldn't read file " + arg)
      }
    }
  }
}
