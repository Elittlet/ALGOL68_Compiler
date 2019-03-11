package compiler

object Parsing {
  object MyParsersNoWhitespace {
    import fastparse.all._

    val digits : Parser[Int] = P (CharIn ('0' to '9').rep (1).!).map (s => s.toInt)
    val integer : Parser[Expr] = digits.map (n => CstI (n))

    val keywords : List[String] = List ("in", "end", "IF", "THEN", "ELSE", "FOR", "TO", "IN", "print", "DO", "OD", "WHILE", "PROC", "printstring", "read", "write", "newarray")
    val alpha : Parser[String] = P ((CharIn ('A' to 'Z') | CharIn ('a' to 'z')).!)
    val ident : Parser[String] = P ((alpha ~ (alpha | CharIn ('0' to '9')).rep (0)).!).filter (s => !keywords.contains (s))
    val variable : Parser[Expr] = ident.map (s => Var (s))
  }

  object MyParsers {
    val White = fastparse.WhitespaceApi.Wrapper {
      import fastparse.all._
      NoTrace (CharIn (" \n\r\t").rep) // Identifies newline characters and tab characters as whitespace
    }



    /* PARSER: IDENTIFYING WHAT TO PARSE AND HOW TO PARSE DIFFERENT STATEMENTS, EXPRESSIONS, AND FUNCTION DECLARATIONS
                ALGOL HAS WIERD ASSIGNMENT STATEMENTS AND FUNCTION DECLARATIONS SO I JUST TAILORED IT TO SATISFACTION    */


    import MyParsersNoWhitespace._
    import White._
    import fastparse.noApi._


    /*Parsing Expressions */

    /// Maps to either a regular variable expression or to a function call
    val atExpr : Parser[Expr] = P (
      integer |  (ident ~ ("(" ~ expr.rep (sep = ",").map (s => s.toList) ~ ")").?).map {
        case (nm, None) => Var (nm)
        case (nm, Some (es)) => Call (nm, es)
      }
        | ("newarray" ~ "(" ~/ expr ~ ")").map (e => NewArray (e)) |
        ("read" ~ "(" ~/ expr ~ "," ~ expr  ~ ")").map { case (arr, idx) => ReadElt (arr, idx) } |
        ("write" ~ "(" ~/ expr ~ "," ~ expr ~ "," ~ expr  ~ ")").map { case (arr, idx, e) => WriteElt (arr, idx, e) } |
        ( "(" ~/ expr ~ ")")
    )



    val multDiv         : Parser[Expr] = P ((atExpr ~ (("*" | "/").! ~ atExpr).rep.map (s => s.toList)).map (foldAssocLeft))
    val addSub          : Parser[Expr] = P ((multDiv ~ (("+" | "-" | "%").! ~ multDiv).rep.map (s => s.toList)).map (foldAssocLeft))
    val gtLtGeLeExpr    : Parser[Expr] = P ((addSub ~ ((">" | "<" | ">=" | "<=").! ~ addSub).rep.map (s => s.toList)).map (foldAssocLeft))
    val eqNeExpr        : Parser[Expr] = P ((gtLtGeLeExpr ~ (("=" | "<>").! ~ gtLtGeLeExpr).rep.map (s => s.toList)).map (foldAssocLeft))
    val expr            : Parser[Expr] = P (eqNeExpr)

    /*Parsing Statements */

    val putYourStuffHere : Parser[Unit] = P ("do some stuff here") // Don't Know why I still have this here
    val assign           : Parser[Stmt] = P((ident ~ ":=" ~ expr ~ ";").map { case (nm, e) => Assign (nm, e) })
    val forState         : Parser[Stmt] = P(("FOR" ~ ident ~ ":=" ~ expr ~ "TO" ~ expr ~ "DO" ~ statement ~ "OD").map { case (nm, e1, e2, s) => For (nm, e1, e2, s) }) // "OD" ends the loop body ~ Weird
    val statement        : Parser[Stmt] = P(((assign | forState | ifState | whileState | printState | returnState | printString).rep).map { case ss => Block (ss.toList) })
    val printState       : Parser[Stmt] = P(("print" ~ "((" ~ expr ~ "))").map { case (e) => Print (e) })
    val printString      : Parser[Stmt] = P(("printstring" ~ "(" ~ expr ~ ")").map { case (e) => PrintString (e) })
    val whileState       : Parser[Stmt] = P(("WHILE" ~ expr ~ "DO:" ~ statement ~ "OD").map { case (e, s) => While (e, s) })
    val returnState      : Parser[Stmt] = P(("return" ~ expr ~ ";").map { case (e) => Return (e) })
    // To close out each part of an IF THEN ELSE statement you have to put FI at the end of each body.. Why not just enclose the body in more parenthesis? lol
    val ifState          : Parser[Stmt] = P(("IF" ~ expr ~ "THEN" ~ statement ~ "FI" ~ "ELSE" ~ statement ~ "FI").map { case (e, s1, s2) => If (e, s1, s2) })

    // Instead of Curly braces, ALGOL68 identifies the start and body of a function or main function with parenthesis, overkill if you ask me
    val funcdef        : Parser[FuncDef] = P(("PROC" ~ ident ~ "=" ~ "(" ~ ident.rep (sep=",").map (s => s.toList) ~ ")" ~ "(" ~ statement ~ ")").map { case (nm, params, body) => FuncDef (nm, params, body) })
    val program        : Parser[Program] = P((funcdef.rep.map (s => s.toList) ~ "main:(" ~ statement ~ ")").map { case (funcdefs, body) => Program (funcdefs, body) })
    val start          : Parser[Program] = P (program ~ End)
  }



  /*                             BUILDING THE ABSTRACT SYNTAX TREE
                                      Basic Expressions
                         Statements (FOR, ASSIGN, WHILE, PRINT, IF, BLOCKS)
                                                                              */

  sealed trait Expr
  case class CstI (n : Int)                                            extends Expr
  case class Var (nm : String)                                         extends Expr
  case class Prim (nm : String, e1 : Expr, e2 : Expr)                  extends Expr
  case class Call (nm : String, es : List[Expr])                       extends Expr
  case class ReadElt (arr : Expr, idx : Expr)                          extends Expr
  case class WriteElt (arr : Expr, idx : Expr, e : Expr)               extends Expr
  case class NewArray (sz : Expr)                                      extends Expr

  sealed trait Stmt
  case class Assign  (nm : String, e : Expr)                           extends Stmt
  case class For     (nm : String, low: Expr, high : Expr, s : Stmt)   extends Stmt
  case class While   (e : Expr, s : Stmt)                              extends Stmt
  case class Print   (e : Expr)                                        extends Stmt
  case class If      (e : Expr, s1 : Stmt, s2 : Stmt)                  extends Stmt
  case class PrintString (arr : Expr)                                  extends Stmt
  case class Block   (ss : List[Stmt])                                 extends Stmt
  case class Return (e : Expr)                                         extends Stmt

  case class FuncDef (nm : String, params : List[String], body : Stmt)

  case class Program (funs : List[FuncDef], main : Stmt)

  def foldAssocLeft (p : (Expr, List[(String,Expr)])) : Expr = {
    p match {
      case (e1, Nil) => e1
      case (e1, (op, e2) :: rest) => foldAssocLeft (Prim (op, e1, e2), rest)
    }
  }

}
