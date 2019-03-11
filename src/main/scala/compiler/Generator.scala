package compiler

import Parsing._
import Printer._

object Generator {
	type Env = Map[String,String]
  type FuncEnv = Map[String,FuncDef]

  val emptyEnv : Env = Map.empty

  var labelCounter : Int = 0
  def newLabel () : String = {
    labelCounter = labelCounter + 1
    "lab%03d".format (labelCounter)
  }

  // Generate x86-64 assembly to evaluate e.
  // Result is at the top of the stack.
  // The following registers may be changed by the generated assembly language: %rax, %rbx, %rsp, %rip
  def compileExpr (e : Expr, env : Env, fenv : FuncEnv) : String = {
    e match {
      case CstI (i)           =>
        "\tpushq\t$%d\n".format (i)
      case Var (x)            =>
        env.get (x) match {
          case None => throw new RuntimeException ("unable to find variable %s in environment".format (x))
          case Some (lab) =>
            "\tpushq\t%s\n".format (lab)
        }
      case Prim (op, e1, e2) => {
        val insts1 = compileExpr (e1, env, fenv)
        val insts2 = compileExpr (e2, env, fenv)
        val push = "\tpushq\t%rax\n"
        def pop (reg : String) = "\tpopq\t%%%s\n".format (reg)
        val instsOp : String = op match {
          case  "+" => "\taddq\t%rbx, %rax\n"
          case  "-" => "\tsubq\t%rbx, %rax\n"
          case  "*" => "\timulq\t%rbx, %rax\n"
          case  "=" => {
            "\tcmpq\t%rbx, %rax\n" +    // sets ZF if ((rax-rbx) = 0) as signed, i.e., (rax = rbx)
            "\tsete\t%al\n" +           // sets low-order byte (%al) of %rax to 1 if ZF is set, otherwise to 0
            "\tmovzbl\t%al, %eax\n"     // extends %al to %rax (recall that assignment to a 32-bit register clears the upper 32-bits of the corresponding 64-bit register)
          }

          case  "%" => {
            "\tcqto\n" +
            "\tidivq\t%rbx\n" +
            "\tmovq\t%rdx, %rax\n"
          }

          // case "<>" => b2i (i1 != i2)
          case  "<" => {
            "\tcmpq\t%rbx, %rax\n" +    // sets SF if ((rax-rbx) < 0) as signed, i.e., (rax < rbx)
            "\tsets\t%al\n" +           // sets low-order byte (%al) of %rax to 1 if SF is set, otherwise to 0
            "\tmovzbl\t%al, %eax\n"     // extends %al to %rax (recall that assignment to a 32-bit register clears the upper 32-bits of the corresponding 64-bit register)
          }
           //case  ">" => b2i (i1 > i2)
          // case "<=" => b2i (i1 <= i2)
          // case ">=" => b2i (i1 >= i2)
          case   _ => throw new RuntimeException ("unknown primitive " + op)
        }
        insts1 +
        insts2 +
        pop ("rbx") +
        pop ("rax") +
        instsOp +
        push
      }
      case Call (nm, es) => {
        es.reverse.map (e => compileExpr (e, env, fenv)).mkString +
        "\tcall\t%s\n".format (nm) +
        "\taddq\t$%d, %%rsp\n".format (es.length * 8) +
        "\tpushq\t%rax\n"
      }
      case NewArray (sz)           => {
        compileExpr (sz, env, fenv) +
        "\tpopq\t%rdi\n" +
        "\timulq\t$8, %rdi\n" +
        "\tcall\tmalloc\n" +
        "\tpushq\t%rax\n"
      }
      case ReadElt (arr, idx) => {
        compileExpr (arr, env, fenv) +
        compileExpr (idx, env, fenv) +
        "\tpopq\t%rbx\n" +
        "\tpopq\t%rax\n" +
        "\tmovq\t(%rax,%rbx,8), %rax\n" +
        "\tpushq\t%rax\n"
      }
      case WriteElt (arr, idx, e) => {
        compileExpr (arr, env, fenv) +
        compileExpr (idx, env, fenv) +
        compileExpr (e, env, fenv) +
        "\tpopq\t%rcx\n" +
        "\tpopq\t%rbx\n" +
        "\tpopq\t%rax\n" +
        "\tmovq\t%rcx, (%rax,%rbx,8)\n" +
        "\tpushq\t$0\n"
      }
    }
  }

  def compileAll (prog : Program, env : Env, fenv : FuncEnv) : String = {
    header () +
    compileFunc (FuncDef ("main", Nil, prog.main), env, fenv) +
    "\n" +
    prog.funs.map (fd => compileFunc (fd, env, fenv)).mkString ("\n") +
    footer (env)
  }

  def header () : String = {
    ""
  }

  def print_string () : String = {
    "\t.text\n" +
    "\t.globl\tprint_string\n" +
    "\t.type\tprint_string, @function\n" +
    "print_string:\n" +
    ".LFB0:\n" +
    "\tpushq\t%rbp\n" +
    "\tmovq\t%rsp, %rbp\n" +
    "\tsubq\t$16, %rsp\n" +
    "\tmovq\t%rdi, -8(%rbp)\n" +
    "\tjmp\t.L2\n" +
    ".L3:\n" +
    "\tmovq\t-8(%rbp), %rax\n" +
    "\tmovq\t(%rax), %rax\n" +
    "\tmovsbl\t%al, %eax\n" +
    "\tmovl\t%eax, %edi\n" +
    "\tcall\tputchar\n" +
    "\taddq\t$8, -8(%rbp)\n" +
    ".L2:\n" +
    "\tmovq\t-8(%rbp), %rax\n" +
    "\tmovq\t(%rax), %rax\n" +
    "\ttestq\t%rax, %rax\n" +
    "\tjne\t.L3\n" +
    "\tleave\n" +
    "\tret\n"
  }

  def footer (env : Env) : String = {
    "\n" +
    print_string () +
    "\n" +
    "\t.section .rodata\n" +
    ".output:\n" +
    "\t.string \"%d\\n\"\n" +
    "\n" +
    (for ((nm1, _) <- env) yield {
      "\t.globl\t%s\n".format (nm1) +
      "\t.data\n".format (nm1) +
      "\t.align\t8\n" +
      "\t.size\t%s, 8\n".format (nm1) +
      "%s:\n".format (nm1) +
      "\t.quad\t0\n" +
      "\n"
    }).mkString
  }

  def compileFunc (func : FuncDef, env : Env, fenv : FuncEnv) : String = {
    val header = {
      "\t.text\n" +
      "\t.globl\t%s\n".format (func.nm) +
      "\t.type\t%s, @function\n".format (func.nm) +
      "%s:\n".format (func.nm) +
      "\tpushq\t%rbp\n" +
      "\tmovq\t%rsp, %rbp\n"
    }
    val footer = {
      "\tpopq\t%rbp\n" +
      "\tret\n"
    }
    var env2 : Env = env
    for ((param, i) <- func.params.zipWithIndex) {
      env2 = env2 + ( (param, "%d(%%rbp)".format ((i + 2) * 8)) )
    }
    header +
    compileStmt (func.body, env2, fenv) +
    footer
  }

  def compileStmt (s : Stmt, env : Env, fenv : FuncEnv) : String = {
    s match {
      case Assign (nm, e)            => {
        env.get (nm) match {
          case None => throw new RuntimeException ("unable to find variable %s in environment".format (nm))
          case Some (lab) =>
            ppStmt ("// ", s) +
            compileExpr (e, env, fenv) +
            "\tpopq\t%rax\n" +
            "\tmovq\t%%rax, %s\n".format (lab)
        }
      }
      case If (e, s1, s2)          =>
        val label1 = newLabel ()
        val label2 = newLabel ()
        val label3 = newLabel ()
        "// %s\n".format (ppExpr (e)) +
        compileExpr (e, env, fenv) +
        "\tpopq\t%rax\n" +
        "\ttestq\t%rax, %rax\n" +
        "\tjne\t%s\n".format (label1) +
        "\tjmp\t%s\n".format (label2) +
        "%s:\n".format (label1) +
        compileStmt (s1, env, fenv) +
        "\tjmp\t%s\n".format (label3) +
        "%s:\n".format (label2) +
        compileStmt (s2, env, fenv) +
        "%s:\n".format (label3)
      case Block (ss)              => {
        def loop (ss2 : List[Stmt]) : String = {
          ss2 match {
            case Nil       => ""
            case s2 :: ss3 => compileStmt (s2, env, fenv) + loop (ss3)
          }
        }
        loop (ss)
      }
      case For (nm, low, high, s)  => {
        val label1 = newLabel ()
        val label2 = newLabel ()
        "// for (%s := %s to %s)\n".format (nm, ppExpr (low), ppExpr (high)) +
        compileExpr (low, env, fenv) +
        "\tpopq\t%rax\n" +
        "\tmovq\t%%rax, (%s)\n".format (nm) +
        "\tjmp\t%s\n".format (label2) +
        "%s:\n".format (label1) +
        compileStmt (s, env, fenv) +
        "\tmovq\t(%s), %%rax\n".format (nm) +
        "\taddq\t$1, %rax\n" +
        "\tmovq\t%%rax, (%s)\n".format (nm) +
        "%s:\n".format (label2) +
        compileExpr (high, env, fenv) +
        "\tpopq\t%rbx\n" +
        "\tmovq\t(%s), %%rax\n".format (nm) +
        "\tcmpq\t%rbx, %rax\n" +
        "\tjle\t%s\n".format (label1)
      }
      case While (e, s)            => {
        val label1 = newLabel ()
        val label2 = newLabel ()
        "// while (%s)\n".format (ppExpr (e)) +
        "\tjmp\t%s\n".format (label2) +
        "%s:\n".format (label1) +
        compileStmt (s, env, fenv) +
        "%s:\n".format (label2) +
        compileExpr (e, env, fenv) +
        "\tpopq\t%rax\n" +
        "\ttestq\t%rax, %rax\n" +
        "\tjne\t%s\n".format (label1)
      }
      case Print (e)               => {
        ppStmt ("// ", s) +
        compileExpr (e, env, fenv) +
        "\tpopq\t%rsi\n" +
        "\tmovl\t$.output, %edi\n" +
        "\tmovl\t$0, %eax\n" +
        "\tcall\tprintf\n"
      }
      case PrintString (e)  => {
        ppStmt ("// ", s) +
        compileExpr (e, env, fenv) +
        "\tpopq\t%rdi\n" +
        "\tcall\tprint_string\n"
      }
       case Return (e)               => {
        ppStmt ("// ", s) +
        compileExpr (e, env, fenv) +
        "\tpopq\t%rax\n" +
        "\tpopq\t%rbp\n" +
        "\tret\n"
      }
    }
  }

  def findVarsExpr (e : Expr) : List[String] = {
    e match {
      case CstI (i)               => Nil
      case Var (x)                => List (x)
      case Prim (op, e1, e2)      => findVarsExpr (e1) ::: findVarsExpr (e2)
      case Call (nm, es)          => es.flatMap (findVarsExpr)
      case NewArray (sz)          => findVarsExpr (sz)
      case ReadElt (arr, idx)     => findVarsExpr (arr) ::: findVarsExpr (idx)
      case WriteElt (arr, idx, e) => findVarsExpr (arr) ::: findVarsExpr (idx) ::: findVarsExpr (e)
    }
  }

  def findVarsStmt (s : Stmt) : List[String] = {
    s match {
      case Assign (nm, e)            => nm :: findVarsExpr (e)
      case If (e, s1, s2)          => findVarsExpr (e) ::: findVarsStmt (s1) ::: findVarsStmt (s2)
      case Block (ss)              => {
        def loop (ss2 : List[Stmt]) : List[String] = {
          ss2 match {
            case Nil       => Nil
            case s2 :: ss3 => findVarsStmt (s2) ::: loop (ss3)
          }
        }
        loop (ss)
      }
      case For (nm, low, high, s)  => {
        nm :: findVarsExpr (low) ::: findVarsExpr (high) ::: findVarsStmt (s)
      }
      case While (e, s)            => {
        findVarsExpr (e) ::: findVarsStmt (s)
      }
      case Print (e)               => {
        findVarsExpr (e)
      }
      case PrintString (e)         => {
        findVarsExpr (e)
      }
      case Return (e)              => {
        findVarsExpr (e)
      }

    }
  }

  def findVars (s : Stmt) : List[String] = {
    findVarsStmt (s).toSet.toList.sortWith ((s1,s2) => s1 < s2)
  }
}
