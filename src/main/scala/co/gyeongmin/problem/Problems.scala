package co.gyeongmin.problem

import scala.annotation.tailrec
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