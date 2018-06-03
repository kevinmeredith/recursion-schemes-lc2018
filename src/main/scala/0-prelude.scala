package lc2018

import matryoshka._
import matryoshka.data._
import matryoshka.implicits._
import scalaz._
import Scalaz._

/**
  * Let's begin with what's probably the simplest possible recursive structure: natural numbers
  *
  * Natural numbers can be defined recursively:
  * A number is either
  *   - zero, noted `Z`
  *   - the successor of a number, noted `S(n) where n is the notation of some number
  *
  * This notation is often referred to as the Peano notation.
  */
object PeanoNumbers {

  /**
    * We want to encode Peano numbers as a recursive type.
    * This encoding will be a type constructor, out so-called "pattern-functor"
    *
    * Hint: there is a type in the standard library that has exactly the structure we want.
    */
  type PeanoNumberF[A] = Option[A]

  /**
    * The problem with the PeanonumberF encoding is that now, different numbers
    * will have different types.
    *
    * We need a fix-point of PeanoNumberF to build a type that can represent all numbers.
    */
  type PeanoNumber = Fix[PeanoNumberF]

  /**
    * Now let's write our very first Algebra! Yay!
    *
    * We want to transform our Peano representation to Int. It's as simple as counting
    * the "layers" of "successor".
    */
  def countLayers: Algebra[PeanoNumberF, Int] = {
    case Some(x) => 1 + x
    case None    => 0
  }

  /**
    * We now have all the ingredients needed to use our first recursion scheme.
    *
    * Hint: this will use the algebra defined above to *destroy* our recursive structure.
    */
  def toInt(peano: PeanoNumber): Int = peano.cata(countLayers)

  /**
    * Now we just need a value to test our functions
    */
  val three: PeanoNumber = Fix(Some(Fix(Some(Fix(Some(Fix[PeanoNumberF](None)))))))

  assert(toInt(three) == 3)
}

/**
  * We now move on to a more interesting recursive structure: the binary tree.
  */
object BinaryTrees {

  sealed trait Tree
  object Tree {
    final case class Branch(label: Int, left: Tree, right: Tree) extends Tree
    final case class Leaf(label: Int)                            extends Tree
    final case class Empty()                                     extends Tree
  }

  /**
    * So the first thing to do is to "translate" our Tree to a pattern-functor.
    * This is done by adding a type parameter and replace each recursive occurrences
    * of Tree by this type parameter in the ADT.
    */
  sealed trait TreeF[A]
  object TreeF {
    final case class Branch[A](label: Int, left: A, right: A) extends TreeF[A]
    final case class Leaf[A](label: Int)                      extends TreeF[A]
    final case class Empty[A]()                               extends TreeF[A]
  }

  /**
    * Of course, we need to have an instance of Functor[TreeF] for it to be a real pattern-functor.
    */
  implicit val treeFFunctor: Functor[TreeF] = new Functor[TreeF] {
    override def map[A, B](fa: TreeF[A])(f: A => B): TreeF[B] = fa match {
      case TreeF.Branch(x, l, r) => TreeF.Branch(x, f(l), f(r))
      case TreeF.Leaf(x)         => TreeF.Leaf(x)
      case TreeF.Empty()         => TreeF.Empty()
    }
  }

  /**
    * It's a good idea to have a pair of (co)algebras that go from Tree to TreeF (and vice versa).
    */
  def treeAlg: TreeF[Tree] => Tree /* Algebra[TreeF, Tree] */ = {
    case TreeF.Empty()         => Tree.Empty()
    case TreeF.Leaf(x)         => Tree.Leaf(x)
    case TreeF.Branch(x, l, r) => Tree.Branch(x, l, r)
  }

  def treeCoalg: Tree => TreeF[Tree] = /*Coalgebra[TreeF, Tree]*/ {
    case Tree.Empty()         => TreeF.Empty()
    case Tree.Branch(x, l, r) => TreeF.Branch(x, l, r)
    case Tree.Leaf(x)         => TreeF.Leaf(x)
  }

  /**
    * These two (co)algebras make it easy to provide a Birecursive instance for Tree/TreeF.
    * This allows to treat Tree as if it were a TreeF, and thus enables to use schemes directly
    * on a Tree (rather than having to wrap it in a fixpoint).
    */
  implicit val treeBirecursive: Birecursive.Aux[Tree, TreeF] = Birecursive.fromAlgebraIso(treeAlg, treeCoalg)

  import Recursive.ops._

  /**
    * A function TreeF[List[Int]] => List[Int]
    *
    * The produced list contains the labels of all the nodes in the tree
    * as enumerated by a depth-first, left-to-right traversal.
    */
  def toList: TreeF[List[Int]] => List[Int] /*Algebra[TreeF, List[Int]]*/ = {
    case TreeF.Branch(x, l, r) => l ++ List(x) ++ r
    case TreeF.Leaf(x)         => List(x)
    case TreeF.Empty()         => Nil
  }

  val testTree: Recursive.AllOps[Tree, TreeF] =
    Tree.Branch(12, Tree.Branch(10, Tree.Leaf(1), Tree.Empty()), Tree.Leaf(15))

  assert(testTree.cata(toList) == List(1, 10, 12, 15))

  /**
    * A function List[Int] => TreeF[List[Int]]
    *
    * This function MUST produce a "sort tree", that is, a tree where each
    * node has a label that is greater than all the labels in its left subtree
    * and lesser than all the labels in its right subtree.
    */
  def fromList: Coalgebra[TreeF, List[Int]] = TODO

  /**
    * I wonder what this mystery function does…
    */
  def mystery(input: List[Int]): List[Int] = input.hylo(toList, fromList)

}
