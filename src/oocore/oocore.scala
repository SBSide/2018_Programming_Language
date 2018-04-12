package oocore

import scala.util.parsing.combinator.JavaTokenParsers

object oocore extends JavaTokenParsers with OpTree {

  // Parser
  def parse(source: String): (Dtree, Ctree) =
    parseAll(prog, source) match {
      case Success(optree, _) => optree
      case _ => throw new Exception("Parse error!")
    }

  def prog: Parser[(Dtree, Ctree)] =
    (defns <~ ";") ~ coms ^^ { case d ~ c => (d, c) } |
      defns ^^ { case d => (d, Seq(List())) }

  def commlist: Parser[List[Ctree]] = rep1sep(comm, ";")

  def coms: Parser[Ctree] = rep1sep(comm, ";") ^^ { case cl => Seq(cl) }
  def comm: Parser[Ctree] =
    lefts ~ ("=" ~> expr) ^^
      { case l ~ e => Assign(l, e) } |
      ("if" ~> expr <~ ":") ~ (coms <~ "else") ~ (coms <~ "end") ^^
      { case e ~ cs1 ~ cs2 => Cond(e, cs1, cs2) } |
      ("while" ~> expr <~ "{") ~ coms <~ "}" ^^
      { case e ~ cs1 => While(e, cs1) } |
      "print" ~> lefts ^^ (Print(_))

  def defns: Parser[Dtree] = rep1sep(defn, ";") ^^ { case dl => Dseq(dl) }
  def defn: Parser[Dtree] =
    ("var" ~> ident) ~ ("=" ~> expr) ^^ { case i ~ e => Dec(i, e) }

  def expr: Parser[Etree] =
    wholeNumber ^^ (Num(_)) |
    "not"~> expr ^^ (Not(_)) |
    ("(" ~> expr <~ "==") ~ expr <~ ")" ^^ { case e1~e2 => Bop(e1,e2)} |
      "(" ~> expr ~ op ~ expr <~ ")" ^^
      {
        case e1 ~ "+" ~ e2 => Add(e1, e2)
        case e1 ~ "-" ~ e2 => Sub(e1, e2)
      } |
      "new" ~> templ ^^ (New(_)) |
      lefts ^^ (Deref(_))

  def templ: Parser[Ttree] =
    "struct" ~> defns <~ "end" ^^ (St(_)) |
      ("array" ~> "[" ~> wholeNumber <~ "]") ~ ("of" ~> expr) ^^
      { case n ~ e => Ar(n.toInt, e) }

  def le: Parser[Ltree] =
    ident ~ rep("[" ~> expr <~ "]") ^^ {
      case l ~ el =>
        val id: Ltree = Id(l)
        (id /: el)(Arr(_, _))
    } |
      ident ^^ (Id(_))

  def lefts: Parser[Ltree] =
    rep1sep(le, ".") ^^
    {
      case h:: List() => h
      case v :: lst => (v /: lst) (Dot(_,_))
      case List() => throw new Exception("empty lhs")
    }

  def op: Parser[String] = "+" | "-"

  sealed abstract class Rval
  case class Handle(loc: Int) extends Rval
  case class Value(n: Int) extends Rval
  case object Nil extends Rval

  sealed abstract class Mem
  case class Namespace(c: Map[String, Rval]) extends Mem
  case class M_Array(n: scala.collection.mutable.ArrayBuffer[Rval]) extends Mem

  var heap: Map[Handle, Mem] = Map()

  var ns: List[Handle] = List()
  ns = List(allocateNS())

  def allocateNS(): Handle = {
    val newhandle = Handle(heap.size)
    val p = if (ns == List()) Handle(-1) else ns.head
    heap += (newhandle -> Namespace(Map("parents" -> p)))
    newhandle
  }

  def heap_of_loc(m: Mem): Map[String, Rval] = m match {
    case Namespace(l) => l
    case _ => throw new Exception("not Namespace")
  }

  def lookup(lval: (Handle, String, Int)): Rval = {
    val (handle, fieldname, i) = lval
    heap(handle) match {
      case Namespace(m) =>
        if ((heap contains handle))
        	heap_of_loc(heap(handle))(fieldname)
        else
        	throw new Exception("lookup error: "+handle+ " Not contains "+ fieldname)
      case M_Array(a) => a(i)
    }
  }

  def store(lval: (Handle, String, Int), rval: Rval): Unit = {
    val (handle, fieldname, i) = lval
    heap(handle) match {
      case Namespace(m) =>
      	if (heap contains handle) {
      		heap += (handle -> Namespace((heap_of_loc(heap(handle)) + (fieldname -> rval))))
      	} else // if the handle is not in the heap
      		throw new Exception("bind error: " + handle + " does not exist in the heap")
      case M_Array(a) =>{
          a(i) = rval
          heap += (handle -> M_Array(a))
      }
    }
  }

  def find(lval: (Handle, String)): Handle = {
    val (han, x) = lval
    if (heap_of_loc(heap(han)) contains x) han
    else {
      heap_of_loc(heap(han))("parents") match {
        case Handle(h) =>
          if (Handle(h) == Handle(-1)) throw new Exception("find error not found " + x)
          else find((Handle(h), x))
        case _ => throw new Exception("This case will never be selected.(find parents)")
      }
    }
  }
  // Interpreter

  def interpretPTREE(p: (Dtree, Ctree)): Unit = {
    val (d, c) = p
    interpretDTREE(d)
    interpretCTREE(c)
  }

  def interpretDTREE(d: Dtree): Unit = d match {
    case Dec(x, e) => {
      store((ns.head, x, -1), interpretETREE(e))
    }
    case Dseq(ds) => for (d <- ds) interpretDTREE(d)
  }
  def interpretTTREE(t: Ttree): Unit = t match {
    case St(d) => interpretDTREE(d)
    case Ar(n, e) =>
      val a = scala.collection.mutable.ArrayBuffer.empty[Rval]
      for (n1 <- 0 to (n - 1)) a += interpretETREE(e)
      heap += (ns.head -> M_Array(a))
  }
  def interpretCLIST(cs: List[Ctree]): Unit =
    for (c <- cs) yield interpretCTREE(c)

  def interpretCTREE(c: Ctree): Unit = c match {
    case Assign(l, e) => {
      store(interpretLTREE(l,ns.head), interpretETREE(e))
    }
    case Cond(e, cs1, cs2) =>
      interpretETREE(e) match {
        case Value(n) => if (n != 0) interpretCTREE(cs1)
        else interpretCTREE(cs2)
        case _ => throw new Exception
      }
    case While(e, cs1) =>
      interpretETREE(e) match {
        case Value(n) =>
          if (n != 0) {
            interpretCTREE(cs1)
            interpretCTREE(c)
          }
        case _ => throw new Exception
      }
    case Print(l) => {
      println(lookup(interpretLTREE(l,ns.head)))
    }
    case Seq(cs) => for (c <- cs) yield interpretCTREE(c)
  }

  def interpretETREE(e: Etree): Rval = e match {
    case Nil_() => Nil
    case Bop(e1,e2)=>{
    	if (interpretETREE(e1)==interpretETREE(e2)) Value(1) else Value(0)
    }
    case Not(e) =>{
      interpretETREE(e) match {
        case Value(n) =>
          if (n==1) Value(0) else Value(1)
        case _ => throw new Exception
      }
    }

    case Num(n) => Value(n.toInt)
    case Add(e1, e2) => {
      val n1 = interpretETREE(e1) match {
        case Value(n) => n
        case _ => throw new Exception (e1+" is not value")
      }
      val n2 = interpretETREE(e2) match {
        case Value(n) => n
        case _ => throw new Exception (e2+" is not value")
      }
      Value(n1 + n2)
    }
    case Sub(e1, e2) =>
      {
        val n1 = interpretETREE(e1) match {
          case Value(n) => n
          case _ => throw new Exception (e1+" is not value")
        }
        val n2 = interpretETREE(e2) match {
          case Value(n) => n
          case _ => throw new Exception (e2+" is not value")
        }
        Value(n1 - n2)
      }
    case Deref(l) => {
      lookup(interpretLTREE(l,ns.head))
    }
    case New(t) => {
      val newhandle = allocateNS()
      ns = List(newhandle) ++ ns
      interpretTTREE(t)
      ns = ns.tail
      newhandle
    }
  }

  def interpretLTREE(left: Ltree,han1:Handle): (Handle, String, Int) = {
    left match {
      case Id(x) => {
        (find(han1, x), x, -1)
      }
      case Dot(ls, l1) => {
        val (han, lval, i) = interpretLTREE(ls,han1)
        lookup(han, lval, i) match {
            case Handle(h) => {
                val (h2,l2,i2) = interpretLTREE(l1,Handle(h))
                heap(Handle(h)) match {
                case Namespace(l) =>
                    if (heap_of_loc(heap(Handle(h))) contains l2) (h2,l2,i2)
                    else throw new Exception("Not member " + l2)
                case M_Array(a) =>
                    a(i2) match {
                        case Handle(h) => (Handle(h), l2, i2)
                        case _ => throw new Exception("array not handle")
                    }
                }
              }
            case _ => throw new Exception("Not handle")
          }
      }
      case Arr(l, e) => {
        val (han, lval, i) = interpretLTREE(l,han1)
        lookup(han, lval, i) match {
          case Handle(h) => {
                interpretETREE(e) match {
                  case Value(n) => (Handle(h), lval, n)
                  case _ => throw new Exception("Not value")
                }
          }
          case _ => throw new Exception(lval + "is not handle")
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    try {
//      val source = "var x =3; var z = new struct var f =0; var g = 1 end; var r = new array[4] of 0;r[3]=100;print r[3]"
//      val source = "var x =3; var z = new struct var f =0; var g = 1 end; var r = new array[4] of 0;z.x=30"
//      val source = "var x =3; var r = new array[4] of 0"
//      val source = "var x =3; var r = new array[4] of 0; r[3]=10"
//      val source = "var x = 7;var y = new array[4] of new struct var a=10 end;y[3].a = 100"
//      val source = "var x = 7;var y = new array[2] of new array[2] of new array[2] of new array[2] of new struct var a=10 end;y[1][1][1][1].a = 100"
//      val source = "var x = 1;var y = new array[2] of new array[2] of new array[2] of new array[2] of new struct var a=x end;y[1][(x-1)][1][1].a = 100"
      val source = "var x =3; var z = new struct var y = new array [3] of new struct var a =10 end end;z.y[0].a=100; print z.y[0].a"
//      val source = "var z = new array [2] of new struct var y = new array [2] of new struct var a =10 end end;z[0].y[1].a=100"
//      val source = "var x = 7;var y = new array[4] of new array[4] of 3;y[3][3]=120;print y[3][3]"
//      val source = "var x = 7;var y = new array[2] of new array[2] of new array[2] of 3 ;y[1][1][1]=120;print y[1][1][1]"
//      val source = "var x = 7;var y = new array[4] of new array[4] of 3;"
//      val source = "var x = 1; var y = new array[2] of new array[2] of new array[2] of 2; y[1][1][x] =x"
//      val source = "var x =3;var z = 2"
      println("input : " + source)
      val optree = parse(source)
      println("optree : " + optree)
      println("done")
      interpretPTREE(optree)
      println("namespace : " + ns)
      println("heap : " + heap)
    } catch { case e: Exception => println(e) }
  }
}
