final class Parser(file:String,line:Int,col:Int,todo:Seq[String]) {
  def takeChar:(Parser,Char) = {
    if (todo.isEmpty) (this, '\0')
    else if (todo.head.isEmpty)
      (new Parser(file, line + 1, 0, todo.tail)).takeChar
    else {
      val next = (todo.head drop 1) +: todo.tail
      val ch   = todo.head(0)
      (new Parser(file,line,col + 1,next), ch)
    }
  }
  
  override def toString = file + ":" + line + ":" + col + ":"
}

final object Parsel {
  import java.io.{File, FileReader, BufferedReader}
  import scala.collection.immutable.Stream

  def unit[A](a:A) = new Parsel[A]((p:Parser) => (p, Right(a)))

  def run[A](contents:String)(f: => Parsel[A]) = 
    f(new Parser("user",0,0,List(contents)))._2

  def run[A](contents:File)(f: => Parsel[A]) = {
    val R = new BufferedReader(new FileReader(contents))
    def inputStream:Stream[String] = {
      val line = R.readLine
      if (line == null) Stream.empty
      else line #:: inputStream
    }
    f(new Parser(contents.getName,0,0,inputStream))
  }

  val digits = "0123456789"
  val upperCase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  val lowerCase = "abcdefghijklmnopqrstuvwxyz"
  val whitespace = " \n\r\f\t"

  def EOF = 
    new Parsel[String]((p:Parser) => {
      val (_, ch) = p.takeChar
      if (ch == '\0') (p, Right("" + ch))
      else (p, Left(p.toString + "expected EOF"))
    })

  def oneOf(charBag:String) = 
    new Parsel[String]((p:Parser) => {
      val (next,ch) = p.takeChar
      if (charBag contains ch) (next, Right("" + ch))
      else (next, Left(p.toString + "expected match from \"" + 
                                      charBag + 
                                      "\""))
    })

  def notOneOf(charBag:String) = 
    new Parsel[String]((p:Parser) => {
      val (next,ch) = p.takeChar
      if (charBag contains ch)
        (next, Left(p.toString + "not expecting match from \"" +
                                 charBag +
                                 "\""))
      else (next, Right("" + ch))
    })

  def manyOf(charBag:String) = 
    new Parsel[String]((p:Parser) => {
      def inner(p:Parser,sofar:String):(Parser,Either[String,String]) = {
        val (next,ch) = p.takeChar
        if (charBag contains ch) inner(next,sofar + ch)
        else (p, Right(sofar))
      }
      inner(p,"")
    })
  
  def noneOf(charBag:String) =
    new Parsel[String]((p:Parser) => {
      def inner(p:Parser,sofar:String):(Parser,Either[String,String]) = {
        val (next,ch) = p.takeChar
        if (charBag contains ch) (p, Right(sofar))
        else inner(next,sofar + ch)
      }
      inner(p,"")
    })

  def atLeastOneOf(charBag:String) = 
    new Parsel[String]((p:Parser) => {
      def inner(cur:Parser,sofar:String):(Parser,Either[String,String]) = {
        val (next,ch) = cur.takeChar
        if (charBag contains ch) inner(next, sofar + ch)
        else if (sofar == "")
          (cur, Left(cur.toString + "expecting at least one of \"" +
                                  charBag + "\""))
        else (cur, Right(sofar))
      }
      inner(p, "")
    })

  def atLeastOneNotOf(charBag:String) =
    new Parsel[String]((p:Parser) => {
      def inner(cur:Parser,sofar:String):(Parser,Either[String, String]) = {
        val (next,ch) = cur.takeChar
        if (charBag contains ch)
          if (sofar == "")
            (cur, Left(cur.toString + "expecting at least one not of \"" +
                                    charBag + "\""))
          else (cur, Right(sofar))
        else inner(next,sofar + ch)
      }
      inner(p, "")
    })

  def exactly(matcher:String) = 
    new Parsel[String]((p:Parser) => {
      def inner(cur:Parser,sofar:String,todo:String)
                       :(Parser,Either[String,String]) = 
        if ((todo == "") && (sofar == matcher))
          (cur, Right(sofar))
        else {
          val (next, ch) = cur.takeChar
          if (ch == todo(0)) inner(next, sofar + ch, todo drop 1)
          else (cur, Left(cur.toString + "expecting an exact match of \"" + 
                                       matcher + "\""))
        }
      inner(p, "", matcher)
    })
}

final class Parsel[A](run:Parser => (Parser,Either[String,A])) {
  def apply(p:Parser) = run(p)
  
  def map[B](f:A => B) =
    new Parsel[B]((p:Parser) => {
      val (parser,result) = run(p)
      result match {
        case Right(v) => Parsel.unit(f(v))(parser)
        case Left(v)  => (p, Left(v))
      }
    })

  def flatMap[B](f:A => Parsel[B]) = 
    new Parsel[B]((p:Parser) => {
      val (parser,result) = run(p)
      result match {
        case Right(v) => f(v)(parser)
        case Left(v)  => (p, Left(v))
      }
    })

  def or(opt:Parsel[A]) =
    new Parsel[A]((p:Parser) => {
      val (parser,result) = run(p)
      result match {
        case Right(_) => (parser,result)
        case Left(_)  => opt(p)
      }
    })

  def and(opt:Parsel[A]) = 
    new Parsel[A]((p:Parser) => {
      val (parser,result) = run(p)
      result match {
        case Right(_) => opt(parser)
        case Left(_)  => (p, result)
      }
    })

  def then[B](opt:Parsel[B]) = 
    new Parsel[B]((p:Parser) => {
      val (parser,_) = run(p)
      opt(parser)
    })
}

object test {
  import Parsel._

  class Expr
  case class ExprInt(val n:Int) extends Expr
  case class ExprFloat(val f:Float) extends Expr
  case class ExprSymbol(val n:String) extends Expr
  case class ExprList(val conses:List[Expr]) extends Expr
  case class ExprOpen() extends Expr
  case class ExprClose() extends Expr

  type Token = Parsel[Expr]

  def matchInt:Token =
    for {
      n <- atLeastOneOf( digits )
    } yield ExprInt(n.toInt)

  def matchFloat:Token = 
    for {
      i <- atLeastOneOf( digits )
      d <- oneOf(".")
      r <- manyOf( digits )
    } yield ExprFloat( (i + d + r).toFloat )

  def matchSymbol:Token = 
    for {
      s  <- oneOf( lowerCase )
      ym <- manyOf( lowerCase + upperCase )
    } yield ExprSymbol(s + ym)

  def matchDelimeter:Token =
    for {
      delim <- oneOf("()")
    } yield delim match {
      case "(" => ExprOpen()
      case ")" => ExprClose()
    }

  def matchList:Token =
    for {
      expr <- matchExpr
      res  <- expr match {
                case ExprClose() => unit(ExprList(Nil))
                case _           => matchList
              }
    } yield expr match {
      case ExprClose() => res
      case _           => res match {
        case ExprList(l) => ExprList( expr :: l )
        case _           => expr
      }
    }

  def matchExpr:Token = 
    for {
      expr <- manyOf( whitespace ) then (
                matchSymbol or
                matchFloat or
                matchInt or
                matchDelimeter )
      res  <- expr match {
                case ExprOpen() => matchList
                case _          => unit(expr)
              }
    } yield res

  def runTest(kind:String,parse:String) = {
    println (kind)
    val str = parse
    val res = run(str) { matchExpr }
    res match {
      case Right(v) => println ("Value found: " + v)
      case Left(v)  => println ("Error found: " + v)
    }
  }

  def test1 = 
    runTest("Aught to pass completely",
            "(some sexpr (add 23 22.5) sTuff (with (embedded lists)))")

  def test2 = 
    runTest("Aught to fail on line 0 and column 6",
            "(some S3xPr (add 23 22.5) sTuff (with (embedded lists)))")
  
  def test3 = 
    runTest("Aught to fail recogonizing a float, and find an int instead",
            "23..63")

  def test4 = 
    runTest("Aught to fail recognizing a symbol",
            "SomeKind3")
}
