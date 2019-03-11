package compiler

import Parsing._

object Printer {
	////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Printing
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def ppExpr (e : Expr) : String = {
    e match {
      case CstI (i)                     => i.toString
      case Var (x)                      => x
      case Prim (op, e1, e2)            => "(%s %s %s)".format (ppExpr (e1), op, ppExpr (e2))
      case Call (nm, es)                => "(%s (%s))".format (nm, es.map (ppExpr).mkString (", "))
      case NewArray (sz)                => "(newarray (%s))".format (ppExpr (sz))
      case ReadElt (arr, idx)           => "(read (%s, %s))".format (ppExpr (arr), ppExpr (idx))
      case WriteElt (arr, idx, e)       => "(write (%s, %s, %s))".format (ppExpr (arr), ppExpr (idx), ppExpr (e))
    }
  }

  def ppBlock (indent : String, s : Stmt) : String = {
    val newIndent = indent + "  "
    s match {
      case Block (ss) => {
        val sb = new StringBuilder
        for (s <- ss) {
          sb.append (ppStmt (newIndent, s))
        }
        sb.toString
      }
      case _ => {
        "%s".format (ppStmt (newIndent, s))
      }
    }
  }

  def ppStmt (indent : String, s : Stmt) : String = {
    val newIndent = indent + "  "
    s match {
      case Assign (nm, e)           => 
        "%s%s := %s;\n".format (indent, nm, ppExpr (e))
      case If (e, s1, s2)         => 
        "%sif (%s) {\n%s%s} else {\n%s%s}\n".format (indent, ppExpr (e), ppBlock (indent, s1), indent, ppBlock (indent, s2), indent)
      case Block (ss) => {
        "%s{\n%s%s}\n".format (indent, ppBlock (indent, s), indent)
      }
      case For (nm, low, high, s) => {
        "%sfor (%s := %s to %s) {\n%s%s}\n".format (indent, nm, ppExpr (low), ppExpr (high), ppBlock (indent, s), indent)
      }
      case While (e, s)           => 
        "%swhile (%s) {\n%s%s}\n".format (indent, ppExpr (e), ppBlock (indent, s), indent)
      case Print (e)              => 
        "%sprint (%s);\n".format (indent, ppExpr (e))
      case PrintString (e)        => 
        "%sprintstring (%s);\n".format (indent, ppExpr (e))
      case Return (e)             => 
        "%sreturn (%s);\n".format (indent, ppExpr (e))
    }
  }

  def ppFuncDef (f : FuncDef) : String = {
    "def %s (%s)\n%s".format (f.nm, f.params.mkString (", "), ppStmt ("", f.body))
  }

  def ppProgram (p : Program) : String = {
    p.funs.map (f => ppFuncDef (f)).mkString ("\n") + ppStmt ("", p.main)
  }

}