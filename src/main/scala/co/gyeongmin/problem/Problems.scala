package co.gyeongmin.problem

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.language.postfixOps
import scala.util.Random

object Problems {

  // P01
  object P01 {
    @tailrec
    def last[A](l: List[A]): A = {
      l match {
        case h :: Nil => h
        case _ :: t => last(t)
        case _ => throw new NoSuchElementException("")
      }
    }
  }

  object P02 {
    // P02
    @tailrec
    def penultimate[A](l: List[A]): A = {
      l match {
        case h :: _ :: Nil => h
        case _ :: t => penultimate(t)
        case _ => throw new NoSuchElementException("")
      }
    }
  }

  object P03 {
    // P03
    @tailrec
    def nth[A](n: Int, l: List[A]): A = {
      l match {
        case h :: _ if n == 0 => h
        case _ :: t if n > 0 => nth(n - 1, t)
        case _ => throw new NoSuchElementException("")
      }
    }
  }

  object P04 {
    // P04
    def length[A](list: List[A]): Int = {
      @tailrec
      def length(list: List[A], n: Int): Int = {
        list match {
          case Nil => n
          case _ :: t => length(t, n + 1)
        }
      }

      length(list, 0)
    }
  }

  object P05 {
    // P05
    def reverse[A](l: List[A]): List[A] = {
      @tailrec
      def reverse(current: List[A], result: List[A]): List[A] = {
        current match {
          case Nil => result
          case h :: t => reverse(t, h :: result)
        }
      }

      reverse(l, Nil)
    }
  }

  object P06 {

    import P05._

    // P06
    def isPalindrome[A](l: List[A]) = l == reverse(l)
  }

  object P07 {
    // P07
    def flatten(l: List[Any]) = {
      @tailrec
      def flatten(l: List[Any], result: List[Any]): List[Any] = {
        l match {
          case Nil => result
          case (h: List[_]) :: t => flatten(h ::: t, result)
          case h :: t => flatten(t, result :+ h)
        }
      }

      flatten(l, Nil)
    }
  }

  object P08 {
    def compress[A](l: List[A]) = {
      @tailrec
      def compress(l: List[A], result: List[A]): List[A] =
        if (l.isEmpty) result
        else {
          val (_, tail) = l span {
            _ == l.head
          }

          compress(tail, result :+ l.head)
        }

      compress(l, Nil)
    }
  }

  object P09 {
    def pack(l: List[Any]) = {
      @tailrec
      def pack(l: List[Any], result: List[Any]): List[Any] =
        if (l.isEmpty) result
        else {
          val (s, rest) = l span {
            _ == l.head
          }
          pack(rest, result :+ s)
        }

      pack(l, Nil)
    }
  }

  object P10 {
    def encode[A](list: List[A]) = {
      @tailrec
      def encode(l: List[A], result: List[(Int, A)]): List[(Int, A)] = {
        if (l.isEmpty) result
        else {
          val (s, rest) = l span {
            _ == l.head
          }
          encode(rest, result :+ (s.length, l.head))
        }
      }

      encode(list, List())
    }
  }

  object P11 {
    def encodeModified[A](list: List[A]) = {
      @tailrec
      def encodeModified(l: List[A], result: List[Any]): List[Any] = {
        if (l.isEmpty) result
        else {
          val (s, rest) = l span {
            _ == l.head
          }
          if (s.length > 1)
            encodeModified(rest, result :+ (s.length, l.head))
          else
            encodeModified(rest, result :+ l.head)
        }
      }

      encodeModified(list, List())
    }
  }

  object P12 {
    def decode[A](list: List[(Int, A)]): List[A] = {
      list.flatMap(item => List.fill(item._1)(item._2))
    }
  }


  object P13 {
    def encodeDirect[A](list: List[A]): List[(Int, A)] = {
      @tailrec
      def encodeDirect(remain: List[A], current: (Int, A), result: List[(Int, A)]): List[(Int, A)] = {
        remain match {
          case Nil => result :+ current
          case head :: tail if head != current._2 =>
            encodeDirect(tail, (1, head), result :+ current)
          case head :: tail if head == current._2 =>
            encodeDirect(tail, (current._1 + 1, head), result)
        }
      }

      encodeDirect(list.tail, (1, list.head), List())
    }
  }

  object P14 {
    def duplicate[A](list: List[A]): List[A] = {
      @tailrec
      def duplicate(list: List[A], remain: List[A]): List[A] = {
        if (list.isEmpty) remain
        else duplicate(list.tail, remain :+ list.head :+ list.head)
      }

      duplicate(list, List())
    }
  }

  object P15 {
    def duplicateN[A](repeat: Int, list: List[A]): List[A] = {
      def duplicate(list: List[A], remain: List[A]): List[A] = {
        if (list.isEmpty) remain
        else duplicate(list.tail, List.fill(repeat)(list.head) ::: remain)
      }

      duplicate(list, List()).reverse
    }
  }

  object P16 {
    def drop[A](n: Int, list: List[A]): List[A] = {
      @tailrec
      def drop(rem: Int, list: List[A], remain: List[A]): List[A] = {
        if (list.isEmpty) remain
        else if (rem <= 1) drop(n, list.tail, remain)
        else drop(rem - 1, list.tail, list.head :: remain)
      }

      drop(n, list, List()).reverse
    }
  }

  object P17 {
    def split[A](n: Int, list: List[A]) = {
      @tailrec
      def split(n: Int, list: List[A], a: List[A]): (List[A], List[A]) = {
        if (list.isEmpty) (a, Nil)
        else if (n < 1) (a, list)
        else split(n - 1, list.tail, a :+ list.head)
      }

      split(n, list, List())
    }
  }

  object P18 {
    import P17._
    def slice[A](n: Int, m: Int, list: List[A]) = {
      split(m - n, split(n, list)._2)._1
    }
  }

  object P19 {
    @tailrec
    def rotate[A](n: Int, list: List[A]): List[A] = {
      val nBounded = if (list.isEmpty) 0 else n % list.length
      if (nBounded < 0) rotate(nBounded + list.length, list)
      else (list drop nBounded) ::: (list take nBounded)
    }
  }

  object P20 {
    def removeAt[A](n: Int, list: List[A]): (List[A], A) =
      ((list take n) ::: (list drop (n + 1)), list(n))
  }

  object P21 {
    def insertAt[A](item: A, n: Int, list: List[A]): List[A] =
      (list take n) ::: (item :: (list drop n))
  }

  object P22 {
    def range(n: Int, m: Int): List[Int] = {
      def range(end: Int, list: List[Int]): List[Int] = {
        if (n > end) list
        else range(end - 1, end :: list)
      }

      range(m, Nil)
    }
  }

  object P23 {
    import P20._
    def randomSelect[A](n: Int, list: List[A]): List[A] = {
      def randomSelect(n: Int, list: List[A], result: List[A]): List[A] = {
        if (n == 0) result
        else {
          val (remain, item) = removeAt(new Random(System.currentTimeMillis()).nextInt(n), list)
          randomSelect(n - 1, remain, item :: result)
        }
      }

      randomSelect(n, list, Nil)
    }
  }

  object P24 {
    import P22._
    import P23._

    def lotto(n: Int, max: Int) = randomSelect(n, range(1, max))
  }

  object P25 {
    import P23._
    def randomPermute[A](items: List[A]) = randomSelect(items.length, items)
  }

  object P26 {
    def extractAndSlice[A](n: Int, list: List[A]): (A, List[A]) = {
      if (list.isEmpty) throw new NoSuchElementException
      else if (n == 0) (list.head, list.tail)
      else extractAndSlice(n - 1, list.tail)
    }

    def combinations[A](n: Int, list: List[A]): List[List[A]] = {
      if (list.isEmpty) throw new NoSuchElementException
      else if (n == 1) list.map(List(_))
      else {
        val last = list.length - n
        val extractAndSliceCurrying: (Int) => (A, List[A]) = extractAndSlice(_, list)
        val combinationsCurrying: ((A, List[A])) => List[List[A]] = {
          case (item, rest) => combinations(n - 1, rest).map(item :: _)
        }

        (0 to last) map extractAndSliceCurrying flatMap combinationsCurrying toList
      }
    }
  }

  object P27 {

    import P26._

    def group3[A](list: List[A]) = {
      for {
        a <- combinations(2, list)
        rem = list diff a
        b <- combinations(3, rem)
      } yield List(a, b, rem diff b)
    }
  }

  object P28 {
    import P26._
    def group[A](ns: List[Int], list: List[A]): List[List[List[A]]] = ns match {
      case Nil => List(Nil)
      case n :: ntail => combinations(n, list) flatMap { choosen =>
        group(ntail, list diff choosen) map { choosen :: _ }
      }
    }
  }

  object P29 {
    def lsort[A](l: List[List[A]]): List[List[A]] = {
      if (l.isEmpty) Nil
      else {
        val pivot = l.head
        val left = lsort(l.filter(_.length < pivot.length))
        val right = lsort(l.filter(_.length > pivot.length))

        val ret: List[List[A]] = if (left.isEmpty && right.isEmpty) List(pivot)
        else if (left.isEmpty && right.nonEmpty) pivot :: right
        else if (left.nonEmpty && right.isEmpty) left :+ pivot
        else (left :+ pivot) ::: right

        ret
      }
    }
  }
  object P30 {
    def lsortFreq[A](l: List[List[A]]): List[List[A]] = {
      if (l.isEmpty) Nil
      else {
        val pivot: List[A] = l.head

        val left: List[List[A]] = lsortFreq(l.filter(_.length > pivot.length))
        val right: List[List[A]] = lsortFreq(l.filter(_.length < pivot.length))

        val ret: List[List[A]] = if (left.isEmpty && right.isEmpty) List(pivot)
        else if (left.isEmpty && right.nonEmpty) pivot :: right
        else if (left.nonEmpty && right.isEmpty) left :+ pivot
        else (left :+ pivot) ::: right

        ret
      }
    }
  }

  object P31 {
    implicit def intExtPrime(n: Int) = new {
      def isPrime: Boolean = {
        !(2 to Math.sqrt(n).toInt).exists(n % _ == 0)
      }
    }
  }

  object P32 {
    @tailrec
    def gcd(n: Int, m: Int): Int = {
      if (n == 0) m
      else if (n > m) gcd(m, n - m)
      else gcd(n, m - n)
    }
  }

  object P33 {
    import P32._
    implicit def extendIsCoprimeTo(n: Int) = new {
      def isCoprimeTo(m: Int) = gcd(n, m) == 1
    }
  }

  object P34 {
    import P33._
    implicit def extendToTotient(n: Int) = new {
      def totient: Int = {
        (1 to n).count(n.isCoprimeTo(_))
      }
    }
  }

  object PrimeSet {
    def isPrime(n: Int): Boolean = {
      !(2 to Math.sqrt(n).toInt).exists(n % _ == 0)
    }

    def primes(n: Int): Stream[Int] = if (isPrime(n)) n #:: primes(n + 1) else primes(n + 1)
    val infPrimes = primes(2)
  }

  object P35 {
    import PrimeSet._

    implicit def extendsToPrimeFactors(n: Int) = new {
      def primeFactors: List[Int] = {
        val primes = infPrimes.takeWhile(_ < n).filter(n % _ == 0).toList

        def repeatMany(n2: Int, p: List[Int], result: List[Int]): List[Int] = {
          p match {
            case Nil =>  result
            case h :: t if n2 % h != 0 => repeatMany(n2, t, result)
            case h :: _ => repeatMany(n2 / h, p, result :+ h)
          }
        }

        repeatMany(n, primes, Nil)
      }
    }
  }

  object P36 {
    import P35._
    implicit def extendsToPrimeFactorMultiplicity(n: Int) = new {
      def primeFactorMultiplicity = {
        n.primeFactors.groupBy((i: Int) => i).mapValues(_.length)
      }
    }
  }

  object P37 {
    import P36._

    implicit def totientExt(n: Int) = new {
      def totient: Int = n.primeFactorMultiplicity.foldLeft(1) { (r, f) =>
        f match {
          case (p, m) => r * (p - 1) * Math.pow(p, m - 1).toInt
        }
      }
    }
  }

  object P39 {
    import PrimeSet._

    def listPrimesinRange(inclusive: Range.Inclusive) =
      infPrimes.dropWhile(_ < inclusive.start).takeWhile(_ <= inclusive.end).toList
  }

  object P40 {
    import P31._
    import PrimeSet._

    implicit def goldbachExt(n: Int) = new {
      def goldbach: (Int, Int) = {
        infPrimes takeWhile { _ < n } find { i => (n - i).isPrime } match {
          case None => throw new IllegalStateException()
          case Some(p) => (p, n - p)
        }
      }
    }
  }

  object P41 {
    import P31._
    import P40._
    import PrimeSet._

    def printGoldbachList(range: Range): Unit = {
      range.filter(_ % 2 == 0).foreach {
        i =>
          val (l, r) = i.goldbach
          println(s"$i = $l + $r")
      }
    }

    def printGoldbachList(range: Range, min: Int): Unit = {
      range.filter(n => n % 2 == 0 && n >= min).filter { num =>
        infPrimes.dropWhile(_ < min).takeWhile(_ < num).find {
          i => {
            (num - i).isPrime && (num - i) >= min
          }
        } match {
          case None => false
          case Some(p) => {
            println(s"$num = $p + ${num - p}")
            true
          }
        }
      }
    }
  }

  object Logics {
    def and(a: Boolean, b: Boolean) = a && b
    def or(a: Boolean, b: Boolean) = a || b
    def nand(a: Boolean, b: Boolean) = !and(a, b)
    def nor(a: Boolean, b: Boolean) = !or(a, b)
    def xor(a: Boolean, b: Boolean) = or(and(a, !b), and(!a, b))
    def impl(a: Boolean, b: Boolean) = or(!a, and(a, b))
    def equ(a: Boolean, b: Boolean) = or(and(a, b), nor(a, b))

    object Implicits {
      implicit def boolExt(a: Boolean) = new {
        def and(b: Boolean) = Logics.and(a, b)
        def or(b: Boolean) = Logics.or(a, b)
        def nand( b: Boolean) = Logics.nand(a, b)
        def nor(b: Boolean) = Logics.nor(a, b)
        def xor(b: Boolean) = Logics.xor(a, b)
        def impl(b: Boolean) = Logics.impl(a, b)
        def equ(b: Boolean) = Logics.impl(a, b)
      }
    }
  }

  object P46 {
    def table2(f: (Boolean, Boolean) => Boolean): Unit = {
      println("A      B      result")
      for {
        a <- List(true, false)
        b <- List(true, false)
      } yield {
        println(s"$a $b ${f(a, b)}")
      }
    }
  }

  object P49 {
    val map: mutable.Map[Int, List[String]] = mutable.Map(0 -> List("0", "1"))

    def makeBaseString(n: Int): List[String] = {
      def makeBaseString(m: Int): List[String] = {
        if (m >= n) {
          map(n)
        } else {
          if (!(map contains (m + 1)))
            map.put(m + 1, map(m) ::: map(m).reverse)

          makeBaseString(m + 1)
        }
      }

      makeBaseString(0)
    }

    import P14._

    def gray(n: Int) = {
      def gray(m: Int, prevStep: List[String]): List[String] = {
        if (m >= n)
          prevStep
        else {
          gray(m + 1,
            (duplicate(prevStep) zip makeBaseString(m)).map {
              case (l, r) => l + r
            })
        }
      }

      gray(0, List(""))
    }
  }

  object P50 {
    abstract class Node[+A](val freq: Int) {
      val comp: Ordering[Node[_]] = {
        Ordering.by((_: Node[_]).freq)
      }
      def +[B >: A](node: Node[B]): NonLeaf[B] = {
        val list: List[Node[B]] = List(this, node)
        NonLeaf(list.min(comp), list.max(comp))
      }
    }

    case class NonLeaf[+A](left: Node[A], right: Node[A]) extends Node[A](left.freq + right.freq)
    case class Leaf[+A](symbol: A, f: Int) extends Node[A](f)

    def encodeTree[A](node: Node[A], strQueue: Queue[String]): List[(A, String)] = {
      node match {
        case NonLeaf(l, r) => encodeTree(l, strQueue.enqueue("0")) ::: encodeTree(r, strQueue.enqueue("1"))
        case Leaf(symbol, _) => List((symbol, strQueue.mkString("")))
      }
    }

    def huffman[A](lists: List[(A, Int)]): List[(A, String)] = {
      def huffman(nodes: List[Node[A]]): List[(A, String)] = {
        if (nodes.length == 1)
          encodeTree(nodes.head, Queue())
        else {
          val (chosen, rest) = nodes.sortBy(_.freq).splitAt(2)
          val cr = chosen.reduce(_ + _)

          huffman(cr :: rest)
        }
      }

      huffman(lists.map {
        case (symbol, freq) => Leaf(symbol, freq)
      })
    }
  }

  sealed abstract class Tree[+T] {
    def isMirrorOf[V](tree: Tree[V]): Boolean
    def isSymmetric: Boolean
    def leafCount: Int
    def leafList: List[T]

    def addValue[U >: T](value: U)(implicit ordimp: U => Ordered[U]): Node[U]
  }
  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString: String = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

    def isMirrorOf[V](tree: Tree[V]): Boolean =
      tree match {
        case t: Node[V] => left.isMirrorOf(t.right) && right.isMirrorOf(t.left)
        case _ => false
      }

    def isSymmetric: Boolean = left.isMirrorOf(right)

    override def addValue[U >: T](value: U)(implicit ordimp: U => Ordered[U]): Node[U] = {
      this match {
        case n@Node(_, End, _) if this.value < value => n.copy(left = Node(value))
        case n@Node(_, _, End) if this.value >= value => n.copy(right = Node(value))
        case _ =>
          if (this.value < value)
            this.copy(left = left.addValue(value))
          else
            this.copy(right = right.addValue(value))
      }
    }

    def leafCount: Int = {
      this match {
        case Node(_, End, End) => 1
        case Node(_, l, r) => l.leafCount + r.leafCount
      }
    }

    def leafList: List[T] = {
      def leafList(node: Node[T]): List[T] = {
        this match {
          case Node(c, End, End) => List(c)
          case Node(_, l, r) => l.leafList ::: r.leafList
        }
      }

      leafList(this)
    }
  }
  case object End extends Tree[Nothing] {
    override def toString = "."

    override def isMirrorOf[V](tree: Tree[V]): Boolean = tree == End

    override def isSymmetric: Boolean = true

    override def addValue[T](value: T)(implicit ordimp: T => Ordered[T]): Node[T] = Node(value)

    override def leafCount: Int = 0

    override def leafList = Nil
  }

  object Node {
    def apply[T](value: T): Node[T] = Node(value, End, End)
  }

  object Tree {
    def treeSize[A](t: Tree[A]): Int = {
      t match {
        case End => 0
        case Node(_, l, r) => 1 + treeSize(l) + treeSize(r)
      }
    }
    def cBalanced[A](n: Int, value: A): List[Tree[A]] = {
      n match {
        case 0 => List(End)
        case _ if n % 2 == 1 =>
          for {
            l <- cBalanced(n / 2, value)
            r <- cBalanced(n / 2, value)
          } yield Node(value, l, r)

        case _ if n % 2 == 0 =>
          val ret = for {
            l <- cBalanced((n - 1) / 2, value)
            r <- cBalanced((n - 1) / 2 + 1, value)
          } yield List(Node(value, l, r), Node(value, r, l))

          ret.flatten
      }
    }

    def fromList[T](list: List[T])(implicit ev$1: T => Ordered[T]): Tree[T] = {
      list.foldLeft(End: Tree[T])(_.addValue(_))
    }

    def symmetricBalancedTrees[A](nodes: Int, value: A): List[Tree[A]] = cBalanced(nodes, value).filter(_.isSymmetric)

    def hbalTrees[A](h: Int, value: A): Node[A] = {
      if (h == 1) Node(value)
      else {
        val node = hbalTrees(h - 1, value)
        Node(value, left = node, right = node)
      }
    }

    def minHbalNodes(height: Int): Int =
      if (height == 0) 0
      else minHbalNodes(height / 2) + 1

    def maxHbalHeight(nodes: Int): Int = {
      val bits = (0 to 4).map(1 << _).foldLeft(nodes) {
        (acc, shifting) => acc | (acc >> shifting)
      }

      val masks = List(0x11111111).flatMap(m => (0 until 4).map(m << _))
      val ret = (0 until 4).map { n =>
        List((bits & masks(n)) >>> n).flatMap {
            item => (0 until 8).map(i => (item >> (4 * i)) & 0xF)
          }.sum
        }.sum

      ret
    }

    def completeBinaryTree[T](n: Int, value: T): Tree[T] = {
      n match {
        case 0 => End
        case n if n % 2 == 0 =>
          Node(value, completeBinaryTree(n / 2, value), completeBinaryTree(n / 2 - 1, value))
        case n if n % 2 == 1 =>
          Node(value, completeBinaryTree(n / 2, value), completeBinaryTree(n / 2, value))
      }
    }
  }

  case class PositionedNode[+T](override val value: T, override val left: Tree[T], override val right: Tree[T], x: Int, y: Int) extends Node[T](value, left, right) {
    override def toString = "T[" + x.toString + "," + y.toString + "](" + value.toString + " " + left.toString + " " + right.toString + ")"
  }

  case class MTree[+T](value: T, children: List[MTree[T]]) {
    def this(value: T) = this(value, List())
    override def toString: String = value.toString + children.map(_.toString).mkString("") + "^"
  }

  object MTree {
    def apply[T](value: T) = new MTree(value, List())
    def apply[T](value: T, children: List[MTree[T]]) = new MTree(value, children)
  }
}

object Main {

}