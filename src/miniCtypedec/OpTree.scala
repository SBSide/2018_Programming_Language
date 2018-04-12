package miniCtypedec

trait OpTree {
  sealed abstract class Ctree
  case class Assign(l: LTree, e: Etree) extends Ctree
  case class While(e: Etree, cs: List[Ctree]) extends Ctree
  case class Print(l: LTree) extends Ctree
  case class Decl(x: String, t: List[String]) extends Ctree

  sealed abstract class Etree
  case class Num(n: String) extends Etree
  case class Add(e1: Etree, e2: Etree) extends Etree
  case class Sub(e1: Etree, e2: Etree) extends Etree
  case class At(l: LTree) extends Etree
  case class Amph(l: LTree) extends Etree

  sealed abstract class LTree
  case class Var(x: String) extends LTree
  case class Star(l: LTree) extends LTree
}
