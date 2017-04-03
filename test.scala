object Compiler {
	import fastparse.all.{Parsed,Parser}

  def readFile (filename : String) : String = {
    val source : scala.io.BufferedSource = io.Source.fromFile (filename)
    try source.getLines.mkString ("\n") finally source.close ()
  }

  def invokeAssemblerLinker (asmFilename : String) : Unit = {
    import scala.sys.process.{Process}
    val pb = Process (List ("gcc", "-o", asmFilename.replace (".s", ""), asmFilename))
    import scala.language.postfixOps
    val result : String = (pb !!)
    println ("Running assembler: %s".format (result))
  }

  def compile (prog : Program, filename: String) : Unit = {
    val fenv : FuncEnv = (for (fd <- prog.funs) yield (fd.nm, fd)).toMap
    val vars : List[String] = for (stmt <- (prog.main :: prog.funs.map (f => f.body)); v <- findVars (stmt)) yield v
    val env : Env = (for (v <- vars) yield (v, "(%s)".format (v))).toMap
    println ("Variables: %s".format (env.mkString (", ")))
    println ("Compiling:")
    val asm : String = compileAll (prog, env, fenv)
    val asmFilename = filename.replace (".A68", ".s")
    val fw = new java.io.FileWriter (asmFilename)
    fw.write (asm)
    fw.close
    println ("Wrote to %s".format (asmFilename))
    invokeAssemblerLinker (asmFilename)
  
  }

  def test (p : Parser[Program], filename : String) : Unit = {
    val input : String = readFile (filename)
    val result : fastparse.core.Parsed[Program, Char, String] = p.parse (input) 
    result match {
      case Parsed.Success (prog, successIndex) => {
        println ("Successfully parsed file \"%s\".\nResult is %s.\nIndex is %d.".format (filename, prog, successIndex))
        
        compile (prog, filename)
      }
      case Parsed.Failure (lastParser, index, extra) => {
        println ("Failed to parse file \"%s\".  Last parser is %s.  Index is %d.  Extra is %s".format (filename, lastParser, index, extra))
      }
    }
  }
}