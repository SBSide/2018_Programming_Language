package oocore

trait OpTree {
  sealed abstract class Ctree
  case class Assign(l: Ltree, e: Etree) extends Ctree
  case class Cond(e: Etree, cs1: Ctree, cs2: Ctree) extends Ctree
  case class While(e: Etree, cs1: Ctree) extends Ctree
  case class Print(l: Ltree) extends Ctree
  case class Seq(cl: List[Ctree]) extends Ctree

  sealed abstract class Etree
  case class Num(s: String) extends Etree
  case class Add(e1: Etree, e2: Etree) extends Etree
  case class Sub(e1: Etree, e2: Etree) extends Etree
  case class Deref(l: Ltree) extends Etree
  case class New(xs: Ttree) extends Etree
  case class Not(e:Etree) extends Etree
  case class Bop(e:Etree,e1:Etree) extends Etree
  case class Nil_() extends Etree

  sealed abstract class Ltree
  case class Id(x: String) extends Ltree
  case class Dot(l: Ltree, x: Ltree) extends Ltree
  case class Arr(l: Ltree, e: Etree) extends Ltree

  sealed abstract class Dtree
  case class Dec(x: String, e: Etree) extends Dtree
  case class Dseq(dl: List[Dtree]) extends Dtree

  sealed abstract class Ttree
  case class St(d: Dtree) extends Ttree
  case class Ar(n: Int, e: Etree) extends Ttree
}
